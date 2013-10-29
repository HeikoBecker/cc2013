#include <sstream>
#include <cstdio>
#include <cctype>
#include "lexer.h"

#define ABORT do {throw std::exception();} while (false)


Token::Token(TokenType type, Pos posinfo): m_type(type), m_posinfo(posinfo) {}

bool Lexer::consumePunctuator() {
	ABORT;
	return false;
}

bool consumeComment() {
	ABORT;
	return false;
}

Lexer::Lexer(FILE* f, char const *name) : posinfo(name, 0, 0)
{
		this->source = f; 
		this->current = EOF;
		this->keywords = std::unordered_set<std::string> {{
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
				} else if (consumePunctuator()) {
					// must readahead to find out which punctuator this is
				} else if (consumeComment()) {
				
				} else {
					// report error
				}
		}
		return std::vector<Token> {};
};
