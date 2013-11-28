#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"

#define ABORT do {throw std::exception();} while (false)

using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(vector<Token> tokens) : m_tokenList(std::move(tokens)) , m_nextsym(m_tokenList.front()) {
  // should you really copy?
  // or is a reference enough
}

bool Parser::parse() {
  auto ok = true;

  // do the parsing here

  return ok;
}

Token Parser::scan() {
  return m_nextsym;
  // TODO: implement this for real
}

void Parser::translationUnit() {
  // TODO: handle more than one externalDeclaration
  externalDeclaration();
}

void Parser::externalDeclaration() {
  functionDefinition();
}


void Parser::functionDefinition() {
  declarationSpecifiers();
  declarator();
  compoundStatement();
}

void Parser::declarationSpecifiers() {
  typeSpecifier();
}

void Parser::typeSpecifier() {
  // TODO
}

void Parser::declarator() {
  // TODO
}

void Parser::compoundStatement() {
  // TODO
}
