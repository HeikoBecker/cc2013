#ifndef PARSER_EXCEPTION_H
#define PARSER_EXCEPTION_H

#include <stdexcept>
#include <string>
#include "pos.h"

class CompilerException: public std::runtime_error {
    public:
      CompilerException(std::string message, Pos where) : std::runtime_error(message), m_where(where) {
        if (m_where.column == 0) {m_where.column = 1;}
        if (m_where.line == 0) {m_where.line = 1;}
      }
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

inline void assert_that(bool that, std::string message)
{
  if(!that)
    throw std::runtime_error(message);
}


#endif
