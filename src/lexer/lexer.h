#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <memory>
#include "../utils/pos.h"
#include "../utils/exception.h"
#include "token.h"

namespace Lexing {

  /*
   * \brief A thin wrapper around fgetc and ungetc which keeps track of the
   * positon in the file.
   * It is used by the lexer class to handle the I/O operations.
   */
  class FileTracker
  {
    public:
      FileTracker(FILE* f, char const *name);
      /* Moves the FILE* to the next character */
      bool advance();
      /* Moves the FILE* to the character before the current */
      void rewind();
      /* Getters for the current char, the current Pos and the last stored Pos*/
      unsigned char current() const {return m_current;}
      Pos currentPosition() const {return m_position;}
      Pos storedPosition() const {return  m_storedPosition;}
      /* Stores the current position in the FILE for later usage */
      void storePosition();
      /* Stores the current position but stores the  @param newpos instead
       * of the current position */
      void storePosition(Pos newpos) {m_storedPosition = newpos;};

    private:
      unsigned char m_current;
      FILE* stream;
      Pos m_position;
      Pos m_storedPosition;
      // additional information, only required to make ungetc work better
      int m_lastCollumn;
      int m_lastChar;
  };

 /*
   * \brief Class implementing the basic lexing functionality
   * If the compiler is invoked with --tokenize it will be used directly by the
   * main function.
   * If any other parameter is given, it will be used as "on-demand" object by
   * the parser to get the next token if needed.
   */
  class Lexer
  {
    public:
      Lexer(FILE* f, char const *name);
      std::shared_ptr<Token> getNextToken();

    private:

      FileTracker tracker;
      std::string curword;
      std::shared_ptr<Token> curtoken;
      /* returns true iff it could consume a Punctuator */
      bool consumePunctuator();
      /* returns true iff it could consume a comment */
      bool consumeComment();
      /* returns true iff it reaches EOF */
      bool consumeWhitespace();
      /* returns true iff it could consume string literal or char constant */
      bool consumeQuoted();
      bool consumeIdentOrDecConstant();
      bool consumeDecimal();
      bool consumeIdent();
      inline void appendToToken(unsigned char c) {curword += c;}
      void storeToken(TokenType type);
      void storeConstToken(ConstantType ct);
      std::shared_ptr<Token> genToken(TokenType type);
  };
  void printToken(const Token & token);
}

#endif
