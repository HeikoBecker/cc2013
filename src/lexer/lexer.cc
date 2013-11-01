#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include "lexer.h"

#define ABORT do {throw std::exception();} while (false)

using namespace Lexing;

// GCC 4.8.1 complains when using auto instead of the explicit type
// this does not make sense :-(
const std::unordered_set<std::string> Lexer::punctuators  = std::unordered_set<std::string> {{
	"[", "]", "(", ")", "{", "}", ".", "->", "++", "--", "&", "*",
	"+", "-", "~", "!", "/", "%", "<<", ">>", "<", ">", "<=", ">=",
	"==", "!=", "^", "|", "&&", "||", "?", ":", ";", "...", "=", 
	"*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=",
	",", "#", "##", "<:", ":>", "<%", "%>", "%:", "%:%:",
}};

const std::unordered_set<std::string> Lexer::keywords = std::unordered_set<std::string> {{
	"_Alignas",
	"_Alignof",
	"_Atomic",
	"_Bool",
	"_Complex",
	"_Generic",
	"_Imaginary",
	"_Noreturn",
	"_Static_assert",
	"_Thread_local",
	"auto",
	"break",
	"case",
	"char",
	"const",
	"continue",
	"default",
	"do",
	"double",
	"else",
	"enum",
	"extern",
	"float",
	"for",
	"goto",
	"if",
	"inline",
	"int",
	"long",
	"register",
	"restrict",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"struct",
	"switch",
	"typedef",
	"union",
	"unsigned",
	"void",
	"volatile",
	"while",
}};

Token::Token(TokenType type, Pos posinfo, std::string value): m_type(type), m_posinfo(posinfo), m_value(value) {}

bool Lexer::consumePunctuator() {
	#define MAXOPLENGTH 3
	auto count_matches = 0;
	auto partial = std::string(1, static_cast<unsigned char>(tracker.current()));
	//TODO: can be done in a more efficient way by checking which operators can
	//actually be part of a "larger" operator
	// check if in punctuator set
	auto matched = false;
	do {
		matched = (punctuators.find(partial) != punctuators.end());
		if (matched) {
			count_matches++;
			if ((tracker.fgetc()) != EOF) {
				partial += static_cast<unsigned char>(tracker.current());
			} else {
				//TODO handle EOF...
			}
		} else if (count_matches > 0) {
			// already had one match, but now got start another token
			tracker.ungetc();
			// remove last character; it was added in the previous
                       // iteration, but not actually part of the punctuator
                       partial.pop_back();
			curword << partial;
			storeToken(TokenType::PUNCTUATOR);
			return true;
		} else {
			return false;
		}
	} while (count_matches < MAXOPLENGTH);
	return false;
	#undef MAXOPLENGTH 
}

bool Lexer::consumeComment() {
	if (tracker.current()== '/') {
		int next = tracker.fgetc();
		if ((next != EOF)) {
			if (next == '*') {
				//found old-style coment
				// consume until */
				while ((tracker.fgetc() != EOF)) {
					if ('*' == tracker.current()) {
						next = tracker.fgetc();
						if ('/' == next) {
							return true;
						} else {
							tracker.ungetc();
						}
					}
				}
                               throw LexingException("Reached end of file while trying to find end of comment", tracker.currentPosition ());
			} else if (next == '/') {
				// found new-style comment
				// consume until newline
				while ((tracker.fgetc() != EOF)) {
					if ('\n' == tracker.current()) {
						return true;
					}
                               }
                               throw LexingException("Reached end of file while trying to find end of comment", tracker.currentPosition ());
                        } else {
                tracker.ungetc(true);
            }
		} else {
                        tracker.ungetc();
		}
	}
	return false;
}

bool Lexer::consumeWhitespace() {
	while (std::isspace(tracker.current())) {
		tracker.fgetc();
	}
	return (tracker.current() == EOF);
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
	while (tracker.fgetc() != EOF) {
		if (tracker.current() == '\\') {
			//start of escape sequence, do a readahead
			if ((tracker.fgetc() == EOF)) {
				// report error, got EOF while waiting for end
				// of escape sequence
				throw LexingException("Reached end of file while looking for escape sequence", tracker.currentPosition());
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
					default:
						//report error
						throw LexingException("Invalid format specifier", tracker.currentPosition());
				}
			}
		} else if (singlequote && tracker.current() == '\'') {
			// end of character constant
			appendToToken(tracker.current());
			// check that constant is not empty, that is, contains
			// more than opening ' and closing '
			if (curword.str().size() <= 2) {
				throw LexingException("Empty character constant", tracker.currentPosition());
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
	throw LexingException("Reached end of file while waiting for closing ???", tracker.currentPosition());
	return true;
}

bool Lexer::consumeIdent() {
	while (tracker.fgetc() != EOF) {
		if (isalpha(tracker.current()) || isdigit(tracker.current()) || '_' == tracker.current()) {
			// create token
			appendToToken(tracker.current());
		} else {
            tracker.ungetc();
			auto isKeyword = (keywords.find(curword.str()) != keywords.end());
			storeToken(isKeyword ? TokenType::KEYWORD : TokenType::IDENTIFIER);
			return true;
		}
	}
	// handle EOf
	return false;
}

bool Lexer::consumeDecimal() {
	while (tracker.fgetc() != EOF) {
		if (isdigit(tracker.current())) {
			appendToToken(tracker.current());
		} else {
			tracker.ungetc();
			storeToken(TokenType::CONSTANT);
			return true;
		}
	}
	return false;
}

bool Lexer::consumeIdentOrDecConstant() {
	if ('0' == tracker.current()) {
		// found 0 constant
		appendToToken('0');
		storeToken(TokenType::CONSTANT);
		return true;
	} else if(isalpha(tracker.current()) || '_' == tracker.current()) {
		appendToToken(tracker.current());
		return consumeIdent();
	} else if (isdigit(tracker.current())) {
		// if it were 0, it would have been catched by the previous rule
		appendToToken(tracker.current());
		return consumeDecimal();
	}
	return false;
}

Lexer::Lexer(FILE* f, char const *name) : tracker(FileTracker(f, name)), curword() {}

std::vector<Token> Lexer::lex() {
		/* delim is the current token deliminator
		 * it is WHITESPACE, except when a (double) quote has been previously
		 * encountered 
		 */
		while ((tracker.fgetc() != EOF)) {
				if (consumeWhitespace()) {
					// reached EOF
					return this->tokens;
				}
				// new token begins after whitespace
				tracker.storePosition();
				if (consumeQuoted()) {
					continue;
				} else if (consumeIdentOrDecConstant()) {
					continue;
				} else if (consumeComment()) {
					continue;
				} else if (consumePunctuator()) {
					continue;
				} else {
					// report error
                                       std::ostringstream msg;
                                       msg << "Got illegal token: " << static_cast<unsigned char>(tracker.current ()) << std::endl;
                                       throw LexingException(msg.str(), tracker.currentPosition ());
				}
		}
		return tokens;
};

void Lexer::storeToken(TokenType type) {
	tokens.push_back(Token(type, tracker.storedPosition(), curword.str()));
	curword.str("");
	curword.clear();
}


FileTracker::FileTracker(FILE* f, char const *name) : stream(f), m_position(Pos(name)), m_storedPosition(Pos(name)) {m_position.line = 1;};

int FileTracker::fgetc() {
        m_lastChar = m_current;
        m_lastCollumn = m_position.column;
	m_current = std::fgetc(stream);
	if ('\n' == m_current) {
		m_position.line++;
		m_position.column = 0;
	} else {
		m_position.column++;
	}
	return m_current;
}

int FileTracker::ungetc(bool reset) {
    if ('\n' == m_current) {
		m_position.line--;
               m_position.column = m_lastCollumn;
	} else {
		m_position.column--;
	}
    auto result =  std::ungetc(m_current, stream);
    if (reset) {
        m_current = m_lastChar;
    }
    return result;
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
						tokentype = "stringliteral";
						break;
		case TokenType::PUNCTUATOR:
						tokentype = "punctuator";
						break;
		default:
						ABORT;
	}
	std::cout << posinfo.name << ":" << posinfo.line << ":" << posinfo.column
					  << " " << tokentype << " " << token.value() << std::endl;
}
