#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include "../pos.h"


namespace Lexing {
  enum class TokenType {
    KEYWORD = 0,
    IDENTIFIER = 1,
    CONSTANT = 2,
    STRINGLITERAL = 3,
    PUNCTUATOR = 4,
    ILLEGAL = 5,
    END = 6
  };

  class Token {
    public:
      Token(TokenType type, Pos posinfo, std::string value); 
      TokenType type() const {return this->m_type;}
      Pos pos() const {return this->m_posinfo;}
      std::string value() const {return this->m_value;}
    private:
      TokenType m_type;
      Pos m_posinfo; 
      std::string m_value;
  };
}

#endif
