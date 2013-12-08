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
    } else if (value == "(") {
      m_puncttype = PunctuatorType::LEFTPARENTHESIS;
    } else if (value == ")" ) {
      m_puncttype = PunctuatorType::RIGHTPARENTHESIS;
    } else if (value == "[" || value == "<:") {
      m_puncttype = PunctuatorType::LEFTSQBRACKET;
    } else if (value == "]" || value == ":>") {
      m_puncttype = PunctuatorType::RIGHTSQBRACKET;
    } else if (value == "{" || value == "<%") {
      m_puncttype = PunctuatorType::LEFTCURLYBRACE;
    } else if (value == "}" || value == "%>") {
      m_puncttype = PunctuatorType::RIGHTCURLYBRACE;
    } else if (value == "<") {
      m_puncttype = PunctuatorType::LESS;
    } else if (value == ">") {
      m_puncttype = PunctuatorType::GREATER;
    } else if (value == "->") {
      m_puncttype = PunctuatorType::ARROW;
    } else if (value == "&") {
      m_puncttype = PunctuatorType::AMPERSAND;
    } else if (value == ";") {
      m_puncttype = PunctuatorType::SEMICOLON;
    } else if (value == ",") {
      m_puncttype = PunctuatorType::COMMA;
    } else {
      /* #, ##, %:, %:%: are missing
       * however, those aren't legal/relevant at the parsing stage anymore
       * */
      m_puncttype = PunctuatorType::ILLEGAL;
    }
  }
