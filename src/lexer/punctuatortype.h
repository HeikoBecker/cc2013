#ifndef LEXER_PUNCTUATOR_H
#define LEXER_PUNCTUATOR_H

enum class PunctuatorType {
  PLUS,    // +
  MINUS,   // -
  STAR,    // *
  ASSIGN,  // =
  EQUAL,   // ==
  NEQUAL,  // !=
  QMARK,   // ?
  COLON,   // :
  LAND,    // &&
  LOR,     // ||
  LESS,    // <
  GREATER, // >
  ARRAY_ACCESS, //[]
  MEMBER_ACCESS, // .
  ARROW, // ->
  ILLEGAL, // TODO: probably not necessary
};
#endif
