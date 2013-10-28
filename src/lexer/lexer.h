#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H
struct FILE;
class Token;

enum TokenType {
		keyword = 0,
		identifier = 1,
		constant = 2,
		string-literal = 3,
		punctuator = 4
}

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
		}

	private:
		FILE* source;	
		static const_expr std::unordered_set<std::string> keywords {{
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

#endif
