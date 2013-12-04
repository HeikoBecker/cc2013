#ifndef LEXER_OPERATOR_H
#define LEXER_OPERATOR_H

enum class OperatorType {
  PLUS,    // +
  MINUS,   // -
  STAR,    // *
  ASSIGN,  // =
  EQUAL,   // ==
  QMARK,   // ?
  COLON,   // :
  LAND,    // &&
  LOR,     // ||
  LESS,    // <
  GREATER,  // >
};
#endif
