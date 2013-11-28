#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"

#define ABORT do {throw std::exception();} while (false)

using namespace std;
using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(vector<Token> tokens) : tokenList(std::move(tokens)) {
  // should you really copy?
  // or is a reference enough
}

bool Parser::parse() {
  bool ok = true;

  // do the parsing here

  return ok;
}
