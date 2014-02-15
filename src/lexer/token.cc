#include "token.h"
#include <iostream>
using namespace Lexing;

Token::Token(TokenType type, Pos posinfo, std::string value)
: m_type(type), m_posinfo(posinfo), m_value(value)
{
  /* FIXME:
   * If we reach EOF, at least colum is 0; In this case, the number is
   * unfortunately not printed at all. Therefore we set it to 1...
   */
  if (m_posinfo.line == 0) {
    m_posinfo.line = 1;
  }
  if (m_posinfo.column == 0) {
    m_posinfo.column = 1;
  }
}

ConstantToken::ConstantToken(Pos posinfo, std::string value, ConstantType ct) 
  : Token(TokenType::CONSTANT, posinfo, value), type(ct) {}


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
    } else if (value == "!") {
      m_puncttype = PunctuatorType::NOT;
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
    } else if (value == ".") {
      m_puncttype = PunctuatorType::MEMBER_ACCESS;
    } else {
      /* #, ##, %:, %:%: are missing
       * however, those aren't legal/relevant at the parsing stage anymore
       * */
      m_puncttype = PunctuatorType::ILLEGAL;
    }
  }

KeywordToken::KeywordToken(TokenType type, Pos posinfo, std::string value)
  : Token(type, posinfo, value) 
{
  if (value == "auto") {
    m_keywordtype = KeywordType::AUTO;
  } else if (value == "break") {
    m_keywordtype = KeywordType::BREAK;
  } else if (value == "case") {
    m_keywordtype = KeywordType::CASE;
  } else if (value == "char") {
    m_keywordtype = KeywordType::CHAR;
  } else if (value == "const") {
    m_keywordtype = KeywordType::CONST;
  } else if (value == "continue") {
    m_keywordtype = KeywordType::CONTINUE;
  } else if (value == "default") {
    m_keywordtype = KeywordType::DEFAULT;
  } else if (value == "do") {
    m_keywordtype = KeywordType::DO;
  } else if (value == "double") {
    m_keywordtype = KeywordType::DOUBLE;
  } else if (value == "else") {
    m_keywordtype = KeywordType::ELSE;
  } else if (value == "enum") {
    m_keywordtype = KeywordType::ENUM;
  } else if (value == "float") {
    m_keywordtype = KeywordType::FLOAT;
  } else if (value == "for") {
    m_keywordtype = KeywordType::FOR;
  } else if (value == "goto") {
    m_keywordtype = KeywordType::GOTO;
  } else if (value == "if") {
    m_keywordtype = KeywordType::IF;
  } else if (value == "int") {
    m_keywordtype = KeywordType::INT;
  } else if (value == "return") {
    m_keywordtype = KeywordType::RETURN;
  } else if (value == "sizeof") {
    m_keywordtype = KeywordType::SIZEOF;
  } else if (value == "struct") {
    m_keywordtype = KeywordType::STRUCT;
  } else if (value == "switch") {
    m_keywordtype = KeywordType::SWITCH;
  } else if (value == "void") {
    m_keywordtype = KeywordType::VOID;
  } else if (value == "while") {
    m_keywordtype = KeywordType::WHILE;
  } else {
    m_keywordtype = KeywordType::WHO_CARES;
  }
}
