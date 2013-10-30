#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <vector>
#include <unordered_set>
#include "../pos.h"

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
		Lexer(FILE* f, char const *name);
		std::vector<Token> lex();
		enum SearchedDelimeter {
		,	WHITESPACE = 0,
			SINGLEQUOTE = 1,
			DOUBLEQUOTE = 2,
		};

	private:
		int current;
		Pos posinfo;
		FILE* source;	
		static std::unordered_set<std::string> keywords;
		/* returns true iff it could consume a Punctuator */
		bool consumePunctuator();
		/* returns true iff it could consume a comment */
		bool consumeComment();
};

class Token
{
	public:
		Token(TokenType type, Pos posinfo); 
		TokenType type() {return this->m_type;}
	private:
		const TokenType m_type;
		const Pos m_posinfo; 
};

#endif
