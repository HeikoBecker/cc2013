#include <sstream>
#include <cstdio>
#include <cctype>
#include "lexer.h"

Lexer::Lexer(FILE* f)
{
		this->source = f; 
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
		int current;
		std::stringstream curword;
		while ((current = fgetc(source) != EOF)) {
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
				if (isspace(current)) {
						// if we're not in a char constant or a string literal,
						// whitespace doesn't matter
						// found token if there was previous input
						// remove consecutive whitespace
						while ((current = fgetc(source) != EOF)) {
								if (!isspace(current)) {
										break;
								}
						}
				}
		}
		return std::vector<Token> {};
};
