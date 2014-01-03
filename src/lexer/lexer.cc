#include <sstream>
#include <iostream>
#include <cstdio>
//#include <cctype>
#include <locale> // for std::isX functions
#include "lexer.h"
#include "token.h"

#define ABORT do {throw std::exception();} while (false)

using namespace Lexing;

namespace {
  bool notDoneYet = false;
}

// GCC 4.8.1 complains when using auto instead of the explicit type
// this does not make sense :-(
const std::unordered_set<std::string> Lexer::punctuators = 
  std::unordered_set<std::string> {{
  "[", "]", "(", ")", "{", "}", ".", "->", "++", "--", "&", "*",
    "+", "-", "~", "!", "/", "%", "<<", ">>", "<", ">", "<=", ">=",
    "==", "!=", "^", "|", "&&", "||", "?", ":", ";", "...", "=", 
    "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=",
    ",", "#", "##", "<:", ":>", "<%", "%>", "%:", "%:%:",
}};

const std::unordered_set<std::string> Lexer::keywords = 
  std::unordered_set<std::string> {{
  "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic",
  "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local", "auto",
  "break", "case", "char", "const", "continue", "default", "do", "double",
  "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int",
  "long", "register", "restrict", "return", "short", "signed", "sizeof",
  "static", "struct", "switch", "typedef", "union", "unsigned", "void",
  "volatile", "while",
}};

bool Lexer::consumePunctuator() {
#define MAXOPLENGTH 3
  auto foundPuntcutor = false;
  auto matched = false;
  static std::string partial;
  if (notDoneYet) {
    foundPuntcutor = true;
    notDoneYet = false;
  } else {
    partial = std::string(1, tracker.current());
  }
  //TODO: can be done in a more efficient way by checking which operators can
  //actually be part of a "larger" operator
  // check if in punctuator set
  do {
    matched = (punctuators.find(partial) != punctuators.end());
    // check also for partial
    if (matched) {
      foundPuntcutor = true;
      if ((tracker.advance())) {
        partial += tracker.current();
      } else {
        curword += partial;
        storeToken(TokenType::PUNCTUATOR);
        return true;
      }
    } else if( partial == "%:%" || partial == "..") {
      // WARNING: tricky code
      // those are not legal punctuators, but part of something that could
      // become legal
      if (tracker.advance()) {
        partial += tracker.current();
        if (punctuators.find(partial) != punctuators.end()) {
          curword = partial;
          storeToken(TokenType::PUNCTUATOR);
          return true;
        }
      }
      notDoneYet = true; // we've read also a part of the next token
      if (partial.substr(0,1) == "..") {
        // partial == ..SOME_CHARACTER, SOME_CHARACTER might be espilon
        partial = partial.substr(1); // partial is now everything after the first dot
        curword = "."; // curword must be . now, else partial wouldn't have been ..
      } else {
        // partial == %:%SOME_CHARACTER
        partial = partial.substr(2); // partial is now everything after %:
        curword += "%:";
      }
      storeToken(TokenType::PUNCTUATOR);
      return true;
    } else if (foundPuntcutor) {
      // already had one match, but now got start another token
      tracker.rewind();
      // remove last character; it was added in the previous
      // iteration, but not actually part of the punctuator
      partial.pop_back();
      curword += partial;
      storeToken(TokenType::PUNCTUATOR);
      return true;
    } else {
      return false;
    }
  } while (true);
#undef MAXOPLENGTH 
}

bool Lexer::consumeComment() {
  if (tracker.current()== '/') {
    if (tracker.advance()) {
      if (tracker.current() == '*') {
        //found old-style coment
        // consume until */
        while ((tracker.advance())) {
          if ('*' == tracker.current()) {
            if (tracker.advance()) {
              if ('/' == tracker.current()) {
                return true;
              } else {
                tracker.rewind();
              }
            }
          }
        }
        throw LexingException(
          "Reached end of file while trying to find end of comment", 
          tracker.currentPosition ()
        );
      } else if (tracker.current() == '/') {
        // found new-style comment
        // consume until newline
        while ((tracker.advance())) {
          if ('\n' == tracker.current()) {
            return true;
          }
        }
        throw LexingException(
          "Reached end of file while trying to find end of comment", 
          tracker.currentPosition ()
        );
      } else { //tracker.current() is neither / nor *
        tracker.rewind();
      }
    } 
  }
  return false;
}

bool Lexer::consumeWhitespace() {
  if (!std::isspace(tracker.current())) return false;
  while (std::isspace(tracker.current())) {
    if (!tracker.advance()) {
      return true; // reached the end of the file
    }
  }
  tracker.rewind();
  return true;
}

bool Lexer::consumeQuoted() {
  bool singlequote = false;
  if ('\'' == tracker.current()) {
    singlequote = true;
  } else if ('\"' != tracker.current()) {
    // text is not quoted
    return false;
  }
  appendToToken(tracker.current());
  while (tracker.advance()) {
    if (tracker.current() == '\\') {
      //start of escape sequence, do a readahead
      if (!tracker.advance()) {
        // report error, got EOF while waiting for end
        // of escape sequence
        throw LexingException(
          "Reached end of file while looking for escape sequence", 
          tracker.currentPosition()
        );
      } else {
        switch (tracker.current()) {
          case '\'':
          case '\"':
          case '\?':
          case '\\':
          case 'a':
          case 'b':
          case 'f':
          case 'n':
          case 'r':
          case 't':
          case 'v':
            //curword << '\\' << tracker.current();
            appendToToken('\\');
            appendToToken(tracker.current());
            break;
          case 'x':
            // read hexadecimal number
            appendToToken('\\');
            appendToToken('x');
            tracker.advance();
            if (std::isxdigit(static_cast<int>(tracker.current()))) {
              appendToToken(tracker.current());
              tracker.advance();
              while (std::isxdigit(static_cast<int>(tracker.current()))) {
                appendToToken(tracker.current());
                tracker.advance();
              }
              tracker.rewind(); //if we got here, we read a non-hexdigit-char
              break;
            } else {
              // we read an x, but it's not followed by a hex digit
            throw LexingException( std::string("Invalid escape sequence, \\x"
                  " was followed by ") + static_cast<char>(tracker.current())
                  + " which is not a hexdigit" 
                , tracker.currentPosition());
            }
            break;
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7': {
              // octal escape sequence
              auto cnt = 1;
              appendToToken('\\');
              appendToToken(tracker.current());
              do {
                tracker.advance();
                if (   isdigit(static_cast<int>(tracker.current()))
                    && tracker.current() != '8'
                    && tracker.current() != '9') {
                  appendToToken(tracker.current());
                } else {
                  tracker.rewind();
                  break;
                }
              } while (cnt <= 3);
            }
            break;
          default:
            //report error
            throw LexingException( std::string("Invalid escape sequence \\") 
                                  + static_cast<char>(tracker.current())
                , tracker.currentPosition());
        }
      }
    } else if ('\n' == tracker.current()) {
      throw LexingException(
        "Found newline in string literal. Maybe you forgot to add a \" or \\n.",
        tracker.currentPosition()
      );
    } else if (singlequote && tracker.current() == '\'') {
      // end of character constant
      appendToToken(tracker.current());
      // check that constant is not empty, that is, contains
      // more than opening ' and closing '
      if (curword.size() <= 2) {
        throw LexingException(
          "Empty character constant", 
          tracker.currentPosition()
        );
      }
      // create Token
      storeToken(TokenType::CONSTANT);
      return true;
    } else if (!singlequote && tracker.current() == '\"') {
      // end of string literal
      appendToToken(tracker.current());
      storeToken(TokenType::STRINGLITERAL);
      return true;
    } else {
      appendToToken(tracker.current());
    }
  }

  // TODO: replace ??? with correct value
  throw LexingException(
    "Reached end of file while waiting for closing ???", 
    tracker.currentPosition()
  );
  return true;
}

bool Lexer::consumeIdent() {
  while (tracker.advance()) {
    if (std::isalpha(tracker.current()) || 
      isdigit(tracker.current()) || 
      '_' == tracker.current()) {
      // create token
      appendToToken(tracker.current());
    } else {
      tracker.rewind();
      auto isKeyword = (keywords.find(curword) != keywords.end());
      storeToken(isKeyword ? TokenType::KEYWORD : TokenType::IDENTIFIER);
      return true;
    }
  }
  // handle EOf
  storeToken(TokenType::IDENTIFIER); // there is no single char keyword
  return true;
}

bool Lexer::consumeDecimal() {
  while (tracker.advance()) {
    if (isdigit(tracker.current())) {
      appendToToken(tracker.current());
    } else {
      if (std::isalpha(tracker.current())) {
        throw LexingException("Decimal constant contains illegal character.", tracker.currentPosition());
      }
      tracker.rewind();
      storeToken(TokenType::CONSTANT);
      return true;
    }
  }
  storeToken(TokenType::CONSTANT);
  return true;
}

bool Lexer::consumeIdentOrDecConstant() {
  if ('0' == tracker.current()) {
    // found 0 constant
    // TODO: is checking for alpha really enough?
    if (tracker.advance()) {
      if (std::isalpha(tracker.current())) {
          throw LexingException(
            "0 constant must not be followed by character.",
            tracker.currentPosition()
            );
          }
      tracker.rewind();
    }
    appendToToken('0');
    storeToken(TokenType::CONSTANT);
    return true;
  } else if(std::isalpha(tracker.current()) || '_' == tracker.current()) {
    appendToToken(tracker.current());
    return consumeIdent();
  } else if (std::isdigit(tracker.current())) {
    // if it were 0, it would have been catched by the previous rule
    appendToToken(tracker.current());
    return consumeDecimal();
  }
  return false;
}

Lexer::Lexer(FILE* f, char const *name) : tracker(FileTracker(f, name)), curword() {}

std::shared_ptr<Token> Lexer::getNextToken() {
  if (notDoneYet) {
    if (consumePunctuator()) {
      throw LexingException("Lexer logic is flawed! This should never happen!\n",
          tracker.currentPosition());
    }
    notDoneYet = false;
    return curtoken;
  }
  do {
    if (!tracker.advance()) {
      return genToken(TokenType::END);
    }
  } while (consumeWhitespace() || consumeComment());

  tracker.storePosition();
  // new token begins after whitespace
  if (consumeQuoted() ||
      consumeIdentOrDecConstant() ||
      consumeComment() ||
      consumePunctuator()) {
    return curtoken;
  } else {
    // report error
    std::ostringstream msg;
    msg << "Got illegal token: " 
      << static_cast<unsigned char>(tracker.current ()) 
      << std::endl;
    throw LexingException(msg.str(), tracker.currentPosition ());
  }
  return genToken(TokenType::END);
}

#if 0
std::vector<Token> Lexer::lex() {
  /* delim is the current token deliminator
   * it is WHITESPACE, except when a (double) quote has been previously
   * encountered 
   */

  std::vector<Token> tokenList = std::vector<Token>();
  while (true) {
    Token token = getNextToken();

    if(token.type() == TokenType::END) {
      tokenList.push_back(token);
      break;
    } else if (token.type() == TokenType::ILLEGAL) {
      // report error
      std::ostringstream msg;
      msg << "Got illegal token: " 
          << static_cast<unsigned char>(tracker.current ()) 
          << std::endl;
      throw LexingException(msg.str(), tracker.currentPosition ());
    } else {
      tokenList.push_back(tokens[0]);
    }
  }
  return tokenList;
};
#endif

std::shared_ptr<Token> Lexer::genToken(TokenType type) {
  if (TokenType::PUNCTUATOR == type) {
    return std::make_shared<PunctuatorToken>(type,
                                             tracker.storedPosition(),
                                             curword);
  } else if (TokenType::KEYWORD == type) {
    return std::make_shared<KeywordToken>(type,
                                             tracker.storedPosition(),
                                             curword);
  }
  return std::make_shared<Token>(type, tracker.storedPosition(), curword);
}

void Lexer::storeToken(TokenType type) {
  curtoken = genToken(type);
  curword.clear();
}

FileTracker::FileTracker(FILE* f, char const *name) 
  : stream(f), m_position(Pos(name)), m_storedPosition(Pos(name)) {
  m_position.line = 1;
  m_position.column = 0;
}

bool FileTracker::advance() {
  auto tmp = std::fgetc(stream);
  if (tmp == EOF) {
    std::ungetc(tmp, stream);
#ifdef DEBUG
    std::cerr << "Reached EOF\n";
#endif
    return false;
  }
#ifdef DEBUG
  std::cerr << "Advancing... "
            << "got " << static_cast<unsigned char>(tmp) << "\n";
#endif
  m_lastChar = m_current;
  m_current = static_cast<unsigned char>(tmp);
  m_lastCollumn = m_position.column;
  if ('\n' == m_current) {
    m_position.line++;
    m_position.column = 0;
  } else {
    m_position.column++;
  }
  return true;
}

void FileTracker::rewind() {
#ifdef DEBUG
  std::cerr << "rewinding...\n";
#endif
  if ('\n' == m_current) {
    m_position.line--;
    m_position.column = m_lastCollumn;
  } else {
    m_position.column--;
  }
  auto result =  std::ungetc(m_current, stream);
  if (result == EOF) {
    ABORT;
  }
  m_current = m_lastChar;
}

void FileTracker::storePosition() {
  m_storedPosition = m_position;
}

void Lexing::printToken(const Token token) {
  auto posinfo = token.pos();
  std::string tokentype {};
  switch (token.type()) {
    case TokenType::KEYWORD:
      tokentype = "keyword";
      break;
    case TokenType::IDENTIFIER:
      tokentype = "identifier";
      break;
    case TokenType::CONSTANT:
      tokentype = "constant";
      break;
    case TokenType::STRINGLITERAL:
      tokentype = "string-literal";
      break;
    case TokenType::PUNCTUATOR:
      tokentype = "punctuator";
      break;

    default:
      ABORT;
  }
  std::cout << posinfo.name << ":" << posinfo.line << ":" << posinfo.column 
            << ": " << tokentype << " " << token.value() 
            << "\n";
}
