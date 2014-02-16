#ifndef PARSER_EXCEPTION_H
#define PARSER_EXCEPTION_H

#include <stdexcept>
#include "pos.h"

class CompilerException: public std::runtime_error {
    public:
      CompilerException(std::string message, Pos where) : std::runtime_error(message), m_where(where) {}
      Pos where() const {return m_where;}
    private:
      Pos m_where;
};

namespace Parsing {

  class ParsingException: public CompilerException 
  {
    public:
      ParsingException(std::string message, Pos pos) 
        : CompilerException(message, pos) {};
  };

}

namespace Lexing {
  class LexingException: public CompilerException 
  {
    public:
      LexingException(std::string message, Pos where)
        : CompilerException(message, where) {};
  };
}


#endif