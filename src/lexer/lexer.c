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
	ABORT;
	auto count_matches = 0;
	auto partial = std::string(1, static_cast<unsigned char>(current));
	//TODO: can be done in a more efficient way by checking which operators can
	//actually be part of a "larger" operator
	// check if in punctuator set
	auto matched = false;
	do {
		matched = (punctuators.find(partial) != punctuators.end());
		if (matched) {
			count_matches++;
			if ((current = fgetc(source)) != EOF) {
				partial += static_cast<unsigned char>(current);
			} else {
				//TODO handle EOF...
			}
		} else if (count_matches > 0) {
			// already had one match, but now got start another token
			ungetc(current, source);
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
	ABORT;
	if (current == '/') {
		int next = fgetc(source);
		if ((next != EOF)) {
			if (next == '*') {
				//found old-style coment
				// consume until */
				while ((current = fgetc(source) != EOF)) {
					if ('*' == current) {
						next = fgetc(source);
						if ('/' == next) {
							return true;
						} else {
							ungetc(next, source);
						}
					}
				}
			} else if (next == '/') {
				// found new-style comment
				// consume until newline
				while ((current = fgetc(source) != EOF)) {
					if ('\n' == current) {
						return true;
					}
				}
			}
		}
		ungetc(next, source);
	}
	return false;
}

Lexer::Lexer(FILE* f, char const *name) : posinfo(name, 0, 0)
{
		this->source = f; 
		this->current = EOF;
}

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
		while ((current = fgetc(source) != EOF), ++posinfo.column) {
				/*if ((current == '\'' && delim == SINGLEQUOTE ) ||*/
				/*(current == '\"' && delim == DOUBLEQUOTE)) {*/
				/*// found string literal or character constant*/
				/*}*/
				if (delim != WHITESPACE) {
						if (current == '\\') {
								//start of escape sequence
								if ((current = fgetc(source) == EOF)) {
										// report error, got EOF while waiting for end
										// of escape sequence
								} else {
										switch (current) { //TODO: implement this
												case '\'':
														break;
												case '\"':
														break;
												case '\?':
														break;
												case '\\':
														break;
												case 'a':
														break;
												case 'b':
														break;
												case 'f':
														break;
												case 'n':
														break;
												case 'r':
														break;
												case 't':
														break;
												case 'v':
														break;
												default:
														//report error
														break;

										}
								}
						} else if (delim == SINGLEQUOTE && current == '\'') {
								// end of character constant
								delim = WHITESPACE;
						} else if (delim == DOUBLEQUOTE && current == '\"') {
								// end of string literal
								delim = WHITESPACE;
						} else {
							// normal character
						}
				}
				if (std::isspace(current)) {
						// if we're not in a char constant or a string literal,
						// whitespace doesn't matter
						// found token if there was previous input
						// reset canBeIdentifier and canBeNumber
						sawAlpha = false;
						sawNumber = false;
						// remove consecutive whitespace
						do {
								if (current == '\n') {
										++posinfo.line;
										posinfo.column = 0;
								} else if (!isspace(current)) {
										break;
								}
						} while ((current = fgetc(source) != EOF), ++posinfo.column); //TODO: dangerous EOF handling
						
				} else if (std::isdigit(current)) {
					// could be part of identifier or decimal constant
					sawNumber = true;
				} else if (std::isalpha(current) || '_' == current) {
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
