#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <iostream>
#include "../lexer/token.h"
#include "../utils/debug.h"

namespace Parsing {

class PrettyPrinter {
  public:
    PrettyPrinter();

    template<typename T> void pprint(T node, unsigned int indentLevel) const {
      node.prettyPrint(*this, indentLevel);
    }

    template<typename S> void pprint(std::shared_ptr<S> nodeptr, unsigned int indentLevel) const 
    {
      if(nodeptr) {
        nodeptr->prettyPrint(*this, indentLevel);
      } else {
        pprint(std::string("\n"), indentLevel);
        pprint(std::string("******************************************\n"),
            indentLevel);
        pprint(std::string("*WARNING: SHARED_PTR WAS NOT INITIALIZED!*\n"),
            indentLevel);
        pprint(std::string("*Should have been ") + typeid(S).name() + "\n",
            indentLevel);
        pprint(std::string("******************************************\n"),
            indentLevel);
        pprint(std::string("\n"),
            indentLevel);
      }
    }
};

template<> inline void PrettyPrinter::pprint<PunctuatorType>(PunctuatorType op, unsigned int indentLevel) const
{
  (void) indentLevel;
  std::cout << Lexing::PunctuatorType2String(op);
}

template<> inline void PrettyPrinter::pprint<char>(char c, unsigned int indentLevel) const 
{
  switch (c) {
    case '\n':
      std::cout << '\n';
      for (int i = indentLevel; i > 0; i--) {
        std::cout << '\t';
      }
      break;
    default:
      std::cout << c;
      break;
  }
}

template<> inline void PrettyPrinter::pprint<std::string>(std::string s, unsigned int indentLevel) const
{
  (void) indentLevel;
  // TODO: replace newline in s with newline + indentLevel times tab char
  std::cout << s;
}

}
#endif
