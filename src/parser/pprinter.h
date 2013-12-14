#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <iostream>
#include "../lexer/punctuatortype.h"

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
  switch (op) {
    case PunctuatorType::PLUS:
      std::cout << "+";
      break;
    case PunctuatorType::MINUS:
      std::cout << "-";
      break;
    case PunctuatorType::STAR:
      std::cout << "*";
      break;
    case PunctuatorType::ASSIGN:
      std::cout << "=";
      break;
    case PunctuatorType::EQUAL:
      std::cout << "==";
      break;
    case PunctuatorType::NEQUAL:
      std::cout << "!=";
      break;
    case PunctuatorType::QMARK:
      std::cout << "?";
      break;
    case PunctuatorType::COLON:
      std::cout << ":";
      break;
    case PunctuatorType::SEMICOLON:
      std::cout << ";";
      break;
    case PunctuatorType::LAND:
      std::cout << "&&";
      break;
    case PunctuatorType::LOR:
      std::cout << "||";
      break;
    case PunctuatorType::LESS:
      std::cout << "<";
      break;
    case PunctuatorType::GREATER:
      std::cout << ">";
      break;
    case PunctuatorType::ARRAY_ACCESS:
      std::cout << "[]";
      break;
    case PunctuatorType::MEMBER_ACCESS:
      std::cout << ".";
      break;
    case PunctuatorType::ARROW:
      std::cout << "->";
      break;
    case PunctuatorType::SIZEOF:
      std::cout << "sizeof";
      break;
    case PunctuatorType::LEFTSQBRACKET:
      std::cout << "[";
      break;
    case PunctuatorType::RIGHTSQBRACKET:
      std::cout << "]";
      break;
    case PunctuatorType::LEFTCURLYBRACE:
      std::cout << "{";
      break;
    case PunctuatorType::RIGHTCURLYBRACE:
      std::cout << "}";
      break;
    case PunctuatorType::COMMA:
      std::cout << ",";
      break;
    case PunctuatorType::AMPERSAND:
      std::cout << "&";
      break;
    case PunctuatorType::NOT:
      std::cout << "!";
      break;
    case PunctuatorType::ILLEGAL:
      std::cout << "ILLEGAL";
      break;
    default:
      std::cout << "ERROR"; //TODO
      break;
  }
}

template<> inline void PrettyPrinter::pprint<char>(char c, unsigned int indentLevel) const 
{
  switch (c) {
    case '\n':
      std::cout << '\n';
      for (int i = indentLevel; i >= 0; i--) {
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
