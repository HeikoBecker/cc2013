#pragma once
#ifndef LEXER_KEYWORD_H
#define LEXER_KEYWORD_H

#include <string>

/*
 * Contains an enum constant for each keyword of the C language
 * Any C keyword not needed for C4 is mapped to WHO_CARES
 * */
enum class KeywordType {
  AUTO,
  BREAK,
  CASE,
  CHAR,
  CONST,
  CONTINUE,
  DEFAULT,
  DO,
  DOUBLE,
  ELSE,
  ENUM,
  FLOAT,
  FOR,
  GOTO,
  IF,
  INT,
  RETURN,
  SIZEOF,
  STRUCT,
  SWITCH,
  VOID,
  WHILE,
  WHO_CARES
};

namespace Lexing {
/* Converts a PunctuatorType to its string representation */
std::string KeywordType2String(const KeywordType kt);
}

#endif
