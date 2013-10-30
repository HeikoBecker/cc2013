#include <sstream>
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

const auto Lexer::punctuators  = std::unordered_set<std::string> {{
	"[", "]", "(", ")", "{", "}", ".", "->", "++", "--", "&", "*",
	"+", "-", "~", "!", "/", "%", "<<", ">>", "<", ">", "<=", ">=",
	"==", "!=", "^", "|", "&&", "||", "?", ":", ";", "...", "=", 
	"*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=",
	",", "#", "##", "<:", ":>", "<%", "%>", "%:", "%:%:",
}};

const auto Lexer::keywords = std::unordered_set<std::string> {{
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

Token::Token(TokenType type, Pos posinfo): m_type(type), m_posinfo(posinfo) {}

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
			// TODO put token into tokenlist?
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

Lexer::Lexer(FILE* f, char const *name) : tracker(FileTracker(f, name)) {}

std::vector<Token> Lexer::lex() {
		std::vector<Token> result;
		/* delim is the current token deliminator
		 * it is WHITESPACE, except when a (double) quote has been previously
		 * encountered 
		 */
		Lexer::SearchedDelimeter delim = WHITESPACE;
		std::stringstream curword;
		bool sawAlpha = false;
		bool sawNumber = false;
		while ((tracker.fgetc() != EOF)) {
				if (delim != WHITESPACE) {
						if (tracker.current() == '\\') {
								//start of escape sequence
								if ((tracker.fgetc() == EOF)) {
										// report error, got EOF while waiting for end
										// of escape sequence
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
														ABORT;
														break;
												default:
														//report error
														break;

										}
								}
						} else if (delim == SINGLEQUOTE && tracker.current() == '\'') {
								// end of character constant
								delim = WHITESPACE;
						} else if (delim == DOUBLEQUOTE && tracker.current() == '\"') {
								// end of string literal
								delim = WHITESPACE;
						} else {
							// normal character
						}
				}
				if (std::isspace(tracker.current())) {
						// if we're not in a char constant or a string literal,
						// whitespace doesn't matter
						// found token if there was previous input
						// reset canBeIdentifier and canBeNumber
						sawAlpha = false;
						sawNumber = false;
						// remove consecutive whitespace
						do {
								//what if current == EOF
						} while ((tracker.fgetc() != EOF)); //TODO: dangerous EOF handling
						
				} else if (std::isdigit(tracker.current())) {
					// could be part of identifier or decimal constant
					sawNumber = true;
				} else if (std::isalpha(tracker.current()) || '_' == tracker.current()) {
					// can only be part of identifier
					if (sawNumber && !sawAlpha) {
						// started to read a number, but now encountered an alhpa
						// report error
						ABORT;
					}
					sawAlpha = true;
				} else if (consumeComment()) {
					// TODO: anything left to do here?
				} else if (consumePunctuator()) {
					// TODO: anything left to do here?
				} else {
					// report error
				}
		}
		return std::vector<Token> {};
};


FileTracker::FileTracker(FILE* f, char const *name) : stream(f), position(Pos(name)) {};

int FileTracker::fgetc() {
	m_current = std::fgetc(stream);
	if ('\n' == m_current) {
		position.line++;
		position.column = 0;
	} else {
		position.column++;
	}
	return m_current;
}

int FileTracker::ungetc() {
	if ('\n' == m_current) {
		// that's tricky; don't do this
		position.line--;
		//position.column = TODO
	} else {
		position.column--;
	}
	return std::ungetc(m_current, stream);
}
