#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H
#pragma once
#include <string>
#include "punctuatortype.h"
#include "keywordtokentype.h"

#include "../utils/pos.h"

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

  class KeywordToken : public Token {
    public:
      KeywordToken(TokenType type, Pos posinfo, std::string value);
      KeywordType keywordtype() const {return this->m_keywordtype;};
    private:
      KeywordType m_keywordtype;
  };

/* Converts a PunctuatorType to its string representation */
// the function and in the header because it is used in an inline templated function
// don't ask
inline std::string PunctuatorType2String(PunctuatorType pt)
{
   switch (pt) {
    case PunctuatorType::PLUS:
      return "+";
    case PunctuatorType::MINUS:
      return "-";
    case PunctuatorType::STAR:
      return "*";
    case PunctuatorType::ASSIGN:
      return "=";
    case PunctuatorType::EQUAL:
      return "==";
    case PunctuatorType::NEQUAL:
      return "!=";
    case PunctuatorType::QMARK:
      return "?";
    case PunctuatorType::COLON:
      return ":";
    case PunctuatorType::SEMICOLON:
      return ";";
    case PunctuatorType::LAND:
      return "&&";
    case PunctuatorType::LOR:
      return "||";
    case PunctuatorType::LESS:
      return "<";
    case PunctuatorType::GREATER:
      return ">";
    case PunctuatorType::ARRAY_ACCESS:
      return "[]";
    case PunctuatorType::MEMBER_ACCESS:
      return ".";
    case PunctuatorType::ARROW:
      return "->";
    case PunctuatorType::SIZEOF:
      return "sizeof";
    case PunctuatorType::LEFTSQBRACKET:
      return "[";
    case PunctuatorType::RIGHTSQBRACKET:
      return "]";
    case PunctuatorType::LEFTCURLYBRACE:
      return "{";
    case PunctuatorType::RIGHTCURLYBRACE:
      return "}";
    case PunctuatorType::COMMA:
      return ",";
    case PunctuatorType::AMPERSAND:
      return "&";
    case PunctuatorType::NOT:
      return "!";
    case PunctuatorType::ILLEGAL:
      return "ILLEGAL";
    default:
      return "ERROR"; //TODO
   }
}

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
