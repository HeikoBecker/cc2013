#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <ostream>
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
    std::ostream& out;

};

template<> inline void PrettyPrinter::pprint<PunctuatorType>(PunctuatorType op) {
  switch (op) {
    case PunctuatorType::PLUS:
      out << " + ";
      break;
    case PunctuatorType::MINUS:
      out << " - ";
      break;
    case PunctuatorType::STAR:
      out << " * ";
      break;
    case PunctuatorType::ASSIGN:
      out << " = ";
      break;
    case PunctuatorType::EQUAL:
      out << " == ";
      break;
    case PunctuatorType::NEQUAL:
      out << " != ";
      break;
    case PunctuatorType::QMARK:
      out << " ? ";
      break;
    case PunctuatorType::COLON:
      out << " : ";
      break;
    case PunctuatorType::SEMICOLON:
      out << " ; ";
      break;
    case PunctuatorType::LAND:
      out << " && ";
      break;
    case PunctuatorType::LOR:
      out << " || ";
      break;
    case PunctuatorType::LESS:
      out << " < ";
      break;
    case PunctuatorType::GREATER:
      out << " > ";
      break;
    case PunctuatorType::ARRAY_ACCESS:
      out << " [] ";
      break;
    case PunctuatorType::MEMBER_ACCESS:
      out << " . ";
      break;
    case PunctuatorType::ARROW:
      out << " -> ";
      break;
    case PunctuatorType::SIZEOF:
      out << " sizeof ";
      break;
    case PunctuatorType::LEFTSQBRACKET:
      out << " [ ";
      break;
    case PunctuatorType::RIGHTSQBRACKET:
      out << " ] ";
      break;
    case PunctuatorType::LEFTCURLYBRACE:
      out << " { ";
      break;
    case PunctuatorType::RIGHTCURLYBRACE:
      out << " } ";
      break;
    case PunctuatorType::COMMA:
      out << " , ";
      break;
    case PunctuatorType::AMPERSAND:
      out << " & ";
      break;
    case PunctuatorType::NOT:
      out << " ! ";
      break;
    case PunctuatorType::ILLEGAL:
      out << " ILLEGAL ";
      break;
    default:
      out << "ERROR"; //TODO
      break;
  }
}

template<> inline void PrettyPrinter::pprint<char>(char c) {
  switch (c) {
    case '\n':
      out << '\n';
      for (int i = indentLevel; i >= 0; i--) {
        out << '\t';
      }
    default:
      out << c;
  }
}

template<> inline void PrettyPrinter::pprint<std::string>(std::string s)
{
  out << s;
}

}
#endif
