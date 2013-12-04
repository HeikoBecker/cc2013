#include "token.h"
using namespace Lexing;

Token::Token(TokenType type, Pos posinfo, std::string value)
: m_type(type), m_posinfo(posinfo), m_value(value) {}


PunctuatorToken::PunctuatorToken(TokenType type, Pos posinfo, std::string value)
  : Token(type, posinfo, value) {
    if (value == "+") {
      m_puncttype = PunctuatorType::PLUS;
    } else if (value == "-") {
      m_puncttype = PunctuatorType::MINUS;
    } else if (value == "*") {
      m_puncttype = PunctuatorType::STAR;
    } else if (value == "=") {
      m_puncttype = PunctuatorType::ASSIGN;
    } else if (value == "==") {
      m_puncttype = PunctuatorType::EQUAL;
    } else if (value == "!=") {
      m_puncttype = PunctuatorType::NEQUAL;
    } else if (value == "?") {
      m_puncttype = PunctuatorType::QMARK;
    } else if (value == ":") {
      m_puncttype = PunctuatorType::COLON;
    } else if (value == "&&") {
      m_puncttype = PunctuatorType::LAND;
    } else if (value == "||") {
      m_puncttype = PunctuatorType::LOR;
    } else if (value == "<") {
      m_puncttype = PunctuatorType::LESS;
    } else if (value == ">") {
      m_puncttype = PunctuatorType::GREATER;
    } else {
      // TODO?
      m_puncttype = PunctuatorType::ILLEGAL;
    }
  }
