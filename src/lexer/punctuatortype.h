#ifndef LEXER_PUNCTUATOR_H
#define LEXER_PUNCTUATOR_H

/* 
 * A mixture of normal punctuators and operators consisting of those 
 * Tokens which are irrelevant for parsing are mapped to ILLEGAL
 * */
enum class PunctuatorType {
  PLUS,    // +
  MINUS,   // -
  STAR,    // *
  ASSIGN,  // =
  EQUAL,   // ==
  NEQUAL,  // !=
  QMARK,   // ?
  COLON,   // :
  SEMICOLON,   // ;
  LAND,    // &&
  LOR,     // ||
  LESS,    // <
  GREATER, // >
  ARRAY_ACCESS, //[]
  MEMBER_ACCESS, // .
  ARROW, // ->
  SIZEOF, // sizeof
  LEFTSQBRACKET, // [
  RIGHTSQBRACKET, // ]
  LEFTPARENTHESIS, // (
  RIGHTPARENTHESIS, // )
  LEFTCURLYBRACE, // {
  RIGHTCURLYBRACE, // }
  COMMA, // ,
  AMPERSAND, // & which genius made this adressof AND binary and?
  ILLEGAL, 
};
#endif
