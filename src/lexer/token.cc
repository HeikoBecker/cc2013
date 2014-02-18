#include "token.h"
#include <iostream>
#include <map>
using namespace Lexing;

Token::Token(TokenType type, Pos posinfo, std::string value)
: m_type(type), m_posinfo(posinfo), m_value(value)
{
}

ConstantToken::ConstantToken(Pos posinfo, std::string value, ConstantType ct) 
  : Token(TokenType::CONSTANT, posinfo, value), type(ct) {}


PunctuatorToken::PunctuatorToken(TokenType type, Pos posinfo, std::string value)
  : Token(type, posinfo, value) {
    static const std::map<std::string, PunctuatorType> lookup
    {
      {"+", PunctuatorType::PLUS},
      {"-", PunctuatorType::MINUS},
      {"*", PunctuatorType::STAR},
      {"=", PunctuatorType::ASSIGN},
      {"==", PunctuatorType::EQUAL},
      {"!=", PunctuatorType::NEQUAL},
      {"!", PunctuatorType::NOT},
      {"?", PunctuatorType::QMARK},
      {":", PunctuatorType::COLON},
      {"&&", PunctuatorType::LAND},
      {"||", PunctuatorType::LOR},
      {"(", PunctuatorType::LEFTPARENTHESIS},
      {")", PunctuatorType::RIGHTPARENTHESIS},
      {"[", PunctuatorType::LEFTSQBRACKET},
      {"<:", PunctuatorType::LEFTSQBRACKET},
      {"]", PunctuatorType::RIGHTSQBRACKET},
      {":>", PunctuatorType::RIGHTSQBRACKET},
      {"{", PunctuatorType::LEFTCURLYBRACE},
      {"<%", PunctuatorType::LEFTCURLYBRACE},
      {"}", PunctuatorType::RIGHTCURLYBRACE},
      {"%>", PunctuatorType::RIGHTCURLYBRACE},
      {"<", PunctuatorType::LESS},
      {">", PunctuatorType::GREATER},
      {"->", PunctuatorType::ARROW},
      {"&", PunctuatorType::AMPERSAND},
      {";", PunctuatorType::SEMICOLON},
      {",", PunctuatorType::COMMA},
      {".", PunctuatorType::MEMBER_ACCESS},
    };
    const std::map<std::string, PunctuatorType>::const_iterator result = lookup.find(value);
    if (result == lookup.cend()) {
      m_puncttype =  PunctuatorType::ILLEGAL;
    } else {
      m_puncttype = result->second;
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
