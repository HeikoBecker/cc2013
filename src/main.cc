#include <stdexcept>

#include "utils/diagnostic.h"
#include "utils/util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <iostream>
#include <fstream>
#include <string>
#include <cstdio>
#include "parser/pprinter.h"
#include "codegen/cogen.h"
#include "utils/debug.h"
#include "utils/exception.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"          /* Nice stacktrace output */
#include "llvm/Support/SystemUtils.h"

enum class Mode {
  TOKENIZE,
  PARSE,
  PRINT_AST,
  COMPILE,
};

int main(int argc, char** const argv)
{
  // speed up I/O
  constexpr auto buffersize = 1024*1024*4;
  static char buffer[buffersize];
  std::setvbuf(stdout, buffer, _IOFBF, sizeof(buffer));

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
                token->type() != Lexing::TokenType::END;
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
            std::string namecpy(name);
            auto last_slash = namecpy.rfind('/');
            auto basename = namecpy.substr(last_slash + 1);
            //                             exclude slash from string
            basename.pop_back(); // remove the c ending
            basename += "ll"; // append ll instead
            auto parser = Parsing::Parser{f, name};
            auto ast = parser.parse();
            Codegeneration::genLLVMIR(basename.c_str(), ast);
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
    for (unsigned int i = 0; i < pos.column-1; i++) {
      if (line[i] == '\t') {
        line += '\t';
      } else {
        line += " ";
      }
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
