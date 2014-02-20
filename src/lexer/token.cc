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
  : Token(type, posinfo, value), m_puncttype(string2punctuator(value)) {
  }

const PunctuatorType & PunctuatorToken::string2punctuator(std::string value) {
    static const std::map<std::string, const PunctuatorType> lookup
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
    static const auto illegal = PunctuatorType::ILLEGAL;
    const std::map<std::string, const PunctuatorType>::const_iterator result = lookup.find(value);
    if (result == lookup.cend()) {
      return illegal;
    } else {
      return result->second;
    }
}

KeywordToken::KeywordToken(TokenType type, Pos posinfo, std::string value)
  : Token(type, posinfo, value), m_keywordtype(string2keyword(value))
{

}

const KeywordType & KeywordToken::string2keyword(std::string value) {
  static const std::map<std::string,const KeywordType> lookup
  {
    {"auto", KeywordType::AUTO},
    {"break", KeywordType::BREAK},
    {"case", KeywordType::CASE},
    {"char", KeywordType::CHAR},
    {"const", KeywordType::CONST},
    {"continue", KeywordType::CONTINUE},
    {"default", KeywordType::DEFAULT},
    {"do", KeywordType::DO},
    {"double", KeywordType::DOUBLE},
    {"else", KeywordType::ELSE},
    {"enum", KeywordType::ENUM},
    {"float", KeywordType::FLOAT},
    {"for", KeywordType::FOR},
    {"goto", KeywordType::GOTO},
    {"if", KeywordType::IF},
    {"int", KeywordType::INT},
    {"return", KeywordType::RETURN},
    {"sizeof", KeywordType::SIZEOF},
    {"struct", KeywordType::STRUCT},
    {"switch", KeywordType::SWITCH},
    {"void", KeywordType::VOID},
    {"while", KeywordType::WHILE},
  };
  static const KeywordType & who_cares = KeywordType::WHO_CARES;
  const std::map<std::string, const KeywordType>::const_iterator result = lookup.find(value);
  if (result == lookup.cend()) {
    return who_cares;
  } else {
    return result->second;
  }
}
