#pragma once
#ifndef LEXER_KEYWORD_H
#define LEXER_KEYWORD_H
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

#endif
