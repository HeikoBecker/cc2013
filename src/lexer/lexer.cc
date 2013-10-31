#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include "lexer.h"

#define ABORT do {throw std::exception();} while (false)

/*TODO:
 * + write a wrapper around fgetc which takes care of handling line and column
 * number
 * + too many while loops with too much EOF checking
 */
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
			} else if (next == '/') {
				// found new-style comment
				// consume until newline
				while ((tracker.fgetc() != EOF)) {
					if ('\n' == tracker.current()) {
						return true;
					}
				}
			}
		}
		tracker.ungetc();
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
				ABORT;
			} else {
				switch (tracker.current()) { //TODO: implement this
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
						ABORT;
						break;
				}
			}
		} else if (singlequote && tracker.current() == '\'') {
			// end of character constant
			appendToToken(tracker.current());
			storeToken(TokenType::CONSTANT);
			// create Token
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

	ABORT;
	return true;
}

bool Lexer::consumeIdent() {
	while (tracker.fgetc() != EOF) {
		if (isalpha(tracker.current()) || isdigit(tracker.current()) || '_' == tracker.current()) {
			// create token
			appendToToken(tracker.current());
		} else {
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
					ABORT;
				}
		}
		return tokens;
};

void Lexer::storeToken(TokenType type) {
	tokens.push_back(Token(type, tracker.storedPosition(), curword.str()));
	curword.str("");
	curword.clear();
}


FileTracker::FileTracker(FILE* f, char const *name) : stream(f), m_position(Pos(name)), m_storedPosition(Pos(name)) {};

int FileTracker::fgetc() {
	m_current = std::fgetc(stream);
	if ('\n' == m_current) {
		m_position.line++;
		m_position.column = 0;
	} else {
		m_position.column++;
	}
	return m_current;
}

int FileTracker::ungetc() {
	if ('\n' == m_current) {
		// that's tricky; don't do this
		m_position.line--;
		//position.column = TODO
	} else {
		m_position.column--;
	}
	return std::ungetc(m_current, stream);
}

void FileTracker::storePosition() {
	m_storedPosition = m_position;
}


void Lexing::printToken(const Token token) {
auto posinfo = token.pos();
	std::cout << posinfo.name << ":" << posinfo.line << ":" << posinfo.column
					  << " " << token.value();
}
