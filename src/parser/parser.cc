#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include "../lexer/lexer.h"

#define ABORT do {throw std::exception();} while (false)

using namespace std;
using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(vector<Token> tokens) {
  // should you really copy?
  // or is a reference enough
  tokenList = tokens;
}

bool Parser::parse() {
  bool ok = true;

  // do the parsing here

  return ok;
}
