#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <unordered_set>
#include <stdexcept>
#include <memory>
#include "../pos.h"
#include "token.h"

namespace Lexing {

  /* TODO: derive from another base exception (e.g. compilerexception) */
  class LexingException: public std::runtime_error 
  {
    public:
      LexingException(std::string message, Pos where) : std::runtime_error(message), m_where(where) {}
      Pos where() const {return m_where;}
    private:
      Pos m_where;
  };

  /*
   * \brief A thin wrapper around fgetc and ungetc which keeps track of the
   * positon in the file
   */
  class FileTracker
  {
    public:
      FileTracker(FILE* f, char const *name);
      int fgetc();
      int ungetc(bool reset = false);
      int current() const {return m_current;}
      Pos currentPosition() const {return m_position;}
      Pos storedPosition() const {return  m_storedPosition;}
      void storePosition();
    private:
      int m_current;
      FILE* stream;
      Pos m_position;
      Pos m_storedPosition;
      // additional information, only required to make ungetc work better
      int m_lastCollumn;
      int m_lastChar;
  };

 /*
   * \brief Class implementing the basic lexing functionality
   * @TODO
   *
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
      const static std::unordered_set<std::string> punctuators;
      const static std::unordered_set<std::string> keywords;
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
      std::shared_ptr<Token> genToken(TokenType type);
  };
  void printToken(const Token token);
}

#endif
