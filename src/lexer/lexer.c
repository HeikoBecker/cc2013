#include <sstream>
#include <cstdio>
#include <cctype>
#include "lexer.h"

Lexer::Lexer(FILE* f)
{
	this->source = f;
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
    while (current = fgetc(source) != EOF) {
	    switch (delim) {
		    case WHITESPACE:
			    if (isspace(current)) {
				    // if we had previous chars, we now got a token
			    } else if ('\'' == current) {
				    // a new character constant begins
				    delim = SINGLEQUOTE;
			    } else if ('\"' == current) {
				    // a new string literal begins
				    delim = DOUBLEQUOTE;
			    }
			    break;
		    case SINGLEQUOTE:
			    if ('\'' == current) {
				    // got a character constant
			    }
			    break;
		    case DOUBLEQUOTE:
			    if ('\"' == current) {
				    // got a string literal
			    }
			    break;
	    }
	    curword << static_cast<unsigned char>(current);

    }
}
