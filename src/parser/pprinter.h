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
      nodeptr->prettyPrint(*this);
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
    case PunctuatorType::QMARK:
      out << " ? ";
      break;
    case PunctuatorType::COLON:
      out << " : ";
      break;
    case PunctuatorType::LAND:
      out << " && ";
      break;
    case PunctuatorType::LOR:
      out << " || ";
      break;
    case PunctuatorType::LESS:
      out << " < ";
    default:
      out << "ERROR"; //TODO
      break;
  }
}

template<> inline void PrettyPrinter::pprint<char>(char c) {
  out << c;
}

template<> inline void PrettyPrinter::pprint<std::string>(std::string s)
{
  out << s;
}

}
#endif
