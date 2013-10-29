#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <vector>
#include <unordered_set>

class Token;

enum TokenType {
		keyword = 0,
		identifier = 1,
		constant = 2,
		stringLiteral = 3,
		punctuator = 4
};

/*
 * \brief Class implementing the basic lexing functionality
 * @TODO
 *
 */
class Lexer
{
	public:
		Lexer(FILE* f);
		std::vector<Token> lex();
		enum SearchedDelimeter {
			WHITESPACE = 0,
			SINGLEQUOTE = 1,
			DOUBLEQUOTE = 2,
		};

	private:
		FILE* source;	
		static std::unordered_set<std::string> keywords;
};

#endif
