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
  ILLEGAL, // TODO: probably not necessary
};
#endif
