#include "token.h"
using namespace Lexing;

Token::Token(TokenType type, Pos posinfo, std::string value)
: m_type(type), m_posinfo(posinfo), m_value(value) {}


OperatorToken::OperatorToken(TokenType type, Pos posinfo, std::string value)
  : Token(type, posinfo, value) {
    if (value == "+") {
      m_optype = OperatorType::PLUS;
    } else if (value == "-") {
      m_optype = OperatorType::MINUS;
    } else if (value == "*") {
      m_optype = OperatorType::STAR;
    } else if (value == "=") {
      m_optype = OperatorType::ASSIGN;
    } else if (value == "==") {
      m_optype = OperatorType::EQUAL;
    } else if (value == "!=") {
      m_optype = OperatorType::NEQUAL;
    } else if (value == "?") {
      m_optype = OperatorType::QMARK;
    } else if (value == ":") {
      m_optype = OperatorType::COLON;
    } else if (value == "&&") {
      m_optype = OperatorType::LAND;
    } else if (value == "||") {
      m_optype = OperatorType::LOR;
    } else if (value == "<") {
      m_optype = OperatorType::LESS;
    } else if (value == ">") {
      m_optype = OperatorType::GREATER;
    } else {
      // TODO?
      m_optype = OperatorType::ILLEGAL;
    }
  }
