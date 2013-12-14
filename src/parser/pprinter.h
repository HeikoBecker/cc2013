#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <iostream>
#include "../lexer/punctuatortype.h"

namespace Parsing {

class PrettyPrinter {
  public:
    PrettyPrinter();
    void addIndentLevel();
    void removeIndentLevel();

    template<typename T> void pprint(T node) {
      node.prettyPrint(*this);
    }

    template<typename S> void pprint(std::shared_ptr<S> nodeptr) {
      if(nodeptr) {
        nodeptr->prettyPrint(*this);
      } else {
        pprint(std::string("\n"));
        pprint(std::string("******************************************\n"));
        pprint(std::string("*WARNING: SHARED_PTR WAS NOT INITIALIZED!*\n"));
        pprint(std::string("*Should have been ") + typeid(S).name() + "\n");
        pprint(std::string("******************************************\n"));
        pprint(std::string("\n"));
      }
    }
  private:
    int indentLevel;

};

template<> inline void PrettyPrinter::pprint<PunctuatorType>(PunctuatorType op) {
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

template<> inline void PrettyPrinter::pprint<char>(char c) {
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

template<> inline void PrettyPrinter::pprint<std::string>(std::string s)
{
  std::cout << s;
}

}
#endif
