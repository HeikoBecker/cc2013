#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H
#pragma once
#include <string>
#include "punctuatortype.h"

#include "../pos.h"

namespace Lexing {
  enum class TokenType {
    KEYWORD = 0,
    IDENTIFIER = 1,
    CONSTANT = 2,
    STRINGLITERAL = 3,
    PUNCTUATOR = 4,
    ILLEGAL = 5,
    END = 6,
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

  class PunctuatorToken : public Token {
    public:
      PunctuatorToken(TokenType type, Pos posinfo, std::string value);
      PunctuatorType punctype() const {return this->m_puncttype;};
    private:
      PunctuatorType m_puncttype;
  };

/*
 *  operator== is NOT virtual; cast a Token to the subclass you want before
 *  using it
 */

inline bool operator==(const Token & lhs, const Token & rhs) {
  if (lhs.type() == rhs.type()) {
    return lhs.value() == rhs.value();
  }
  return false;
}

inline bool operator==(const PunctuatorToken & lhs, const PunctuatorToken & rhs) {
  return lhs.punctype() == rhs.punctype();
}

inline bool operator==(const PunctuatorToken & lhs, const PunctuatorType p) {
  return lhs.punctype() == p;
}

}

#endif
