#include <stdexcept>

#include "diagnostic.h"
#include "util.h"
#include "lexer/lexer.h"
#include "parser/parser.h"
#include <iostream>
#include "parser/pprinter.h"

enum class Mode {
  TOKENIZE,
  PARSE,
  PRINT_AST,
  COMPILE,
};

int main(int, char** const argv)
{
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
        errorf("unknown argument '%s'", arg);
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
#ifdef DEBUG
            if (parser.parse()) {
              printf("PARSING SUCCESSFUL\n");
            } else {
              PANIC("PARSING FAILED\n");
            }
#else
            parser.parse();
#endif
            break;
          }
          case Mode::PRINT_AST:
          {
            auto parser = Parsing::Parser{f, name};
            auto ast = parser.parse();
            auto pp = Parsing::PrettyPrinter();
            pp.pprint(ast, 0);
            break;
          }
          case Mode::COMPILE:
            PANIC("TODO implement");
            break;
        }

        if (f != stdin)
          fclose(f);
      }
    }
  } catch (Parsing::ParsingException const& e) {
    errorf(e.pos, e.what());
    /* no need to handle it; TODO: avoid throwing an exception at all */
  } catch (Lexing::LexingException const& e) {
    errorf(e.where(), e.what());
    //std::cerr << e.where().name << ":" << e.where().line << ":" << e.where().column << ": error: lexing error!"
      //<< "\n"
      //<< "Encountered fatal error:" << "\n"
      //<< e.what()
      //<< "\n";
  } catch (std::exception const& e) {
    errorf("caught exception: %s", e.what());
  } catch (...) {
    errorf("caught unknown exception");
  }

  return printDiagnosticSummary();
}
