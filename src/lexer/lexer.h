#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <vector>
#include <unordered_set>
#include "../pos.h"

namespace Lexing {

class Token;

/*
 * \brief A thin wrapper around fgetc and ungetc which keeps track of the
 * positon in the file
 */
class FileTracker
{
	public:
		FileTracker(FILE* f, char const *name);
		int fgetc();
		int ungetc();
		int current() const {return m_current;}
		Pos storedPosition() const {return  m_storedPosition;}
		void storePosition();
	private:
		int m_current;
		FILE* stream;
		Pos m_position;
		Pos m_storedPosition;
};

enum class TokenType {
		KEYWORD = 0,
		IDENTIFIER = 1,
		CONSTANT = 2,
		STRINGLITERAl = 3,
		PUNCTUATOR = 4,
		ILLEGAL = 5
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
			WHITESPACE = 0,
			SINGLEQUOTE = 1,
			DOUBLEQUOTE = 2,
		};

	private:
		FileTracker tracker;
		const static std::unordered_set<std::string> punctuators;
		const static std::unordered_set<std::string> keywords;
		/* returns true iff it could consume a Punctuator */
		bool consumePunctuator();
		/* returns true iff it could consume a comment */
		bool consumeComment();
};

class Token
{
	public:
		Token(TokenType type, Pos posinfo, std::string value); 
		TokenType type() {return this->m_type;}
	private:
		const TokenType m_type;
		const Pos m_posinfo; 
		const std::string m_value;
};

}

#endif
