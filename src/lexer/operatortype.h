#ifndef LEXER_OPERATOR_H
#define LEXER_OPERATOR_H

enum class OperatorType {
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
