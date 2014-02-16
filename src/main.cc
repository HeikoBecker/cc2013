#include <stdexcept>

#include "utils/diagnostic.h"
#include "utils/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <iostream>
#include <fstream>
#include <string>
#include "parser/pprinter.h"
#include "codegen/cogen.h"
#include "utils/debug.h"
#include "utils/exception.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"          /* Nice stacktrace output */
#include "llvm/Support/SystemUtils.h"

// TODO: use a more portable alternative
#include <libgen.h>

enum class Mode {
  TOKENIZE,
  PARSE,
  PRINT_AST,
  COMPILE,
};

int main(int argc, char** const argv)
{
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);

  try {
    char** i = argv + 1;

    Mode mode = Mode::COMPILE;
    for (; auto const arg = *i; ++i) {
      if (arg[0] != '-') {
        break;
      } else if (strEq(arg, "--tokenize")) {
        mode = Mode::TOKENIZE;
      } else if (strEq(arg, "--parse")) {
        mode = Mode::PARSE;
      } else if (strEq(arg, "--print-ast")) {
        mode = Mode::PRINT_AST;
      } else if (strEq(arg, "--compile")) {
        mode = Mode::COMPILE;
      } else if (strEq(arg, "-")) {
        break;
      } else if (strEq(arg, "--")) {
        ++i;
        break;
      } else {
        errorf("unknown arguments '%s', use --tokenize|parse|print-ast|compile", arg);
      }
    }

    if (!*i)
      errorf("no input files specified");

    if (!hasNewErrors()) {
      for (; char const *name = *i; ++i) {
        FILE* f;
        if (strEq(name, "-")) {
          f    = stdin;
          name = "<stdin>";
        } else {
          f = fopen(name, "r");
          if (!f)
            errorErrno(Pos(name));
        }

        if (hasNewErrors())
          continue;

        Lexing::Lexer lexer{f, name};
        switch (mode) {
          case Mode::TOKENIZE:
            for(auto token = lexer.getNextToken(); 
                token->type() != TokenType::END;
                token = lexer.getNextToken()) {
              printToken(*token);
            }
            break;
          case Mode::PARSE:
          {
            auto parser = Parsing::Parser{f, name};
            if (parser.parse()) {
              debug(GENERAL) << "PARSING SUCCESSFUL\n";
            } else {
              PANIC("PARSING FAILED\n");
            }
            break;
          }
          case Mode::PRINT_AST:
          {
            auto parser = Parsing::Parser{f, name};
            auto ast = parser.parse();
            pprint(ast, 0);
            break;
          }
          case Mode::COMPILE:
            // we need to build the name of the output file
            // TODO: use a less-hacky, more general solution
            // 1024 chars have to be enough for anyone
            char namecpy[1024] = {0};
            // insert rant about strcpy, strlcpy and strncpy
            strncpy(namecpy, name, 1023); //1023 to ensure last char is 0
            auto len = strlen(namecpy);
            if (len < 1023) { // else we have no chance anyway
              namecpy[len-1] = 'l';
              namecpy[len] = 'l';
              namecpy[len+1] = '\0';
            }
            auto output_filename = basename(namecpy);
            auto parser = Parsing::Parser{f, name};
            auto ast = parser.parse();
            Codegeneration::genLLVMIR(output_filename, ast);
            break;
        }

        if (f != stdin)
          fclose(f);
      }
    }
  } catch (CompilerException const& e) {
    errorf(e.where(), e.what());
    auto pos = e.where();
    std::ifstream infile(pos.name);
    auto counter = pos.line;
    std::string line;
    while (counter--) {
      line.clear();
      if (!std::getline(infile, line)) {
        std::cerr << "line number is wrong!" << "\n" << std::endl;
        break;
      }
    }
    infile.close();
    line += '\n';
    for (int i = pos.column-1; i > 0; --i) {
      line += " ";
    }
    line += "^^^^";
    std::cerr << line << std::endl;
  } catch (std::exception const& e) {
    errorf("caught exception: %s", e.what());
  } catch (...) {
    errorf("caught unknown exception");
  }

  return printDiagnosticSummary();
}
