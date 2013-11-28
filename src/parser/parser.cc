#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"

#define ABORT(X) do {throw std::exception();} while (false)

using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(vector<Token> tokens) : m_tokenList(std::move(tokens)) , m_nextsym(m_tokenList.front()) {
  // should you really copy?
  // or is a reference enough
}

bool Parser::parse() {
  auto ok = true;

  /*This is the entry point*/
  scan();
  translationUnit();
  if (m_nextsym.type() != TokenType::END) {
    ABORT();
  }
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
  if (m_nextsym.type() == TokenType::KEYWORD && m_nextsym.value() == "void") {
    scan();
  } else if (m_nextsym.type() == TokenType::KEYWORD && m_nextsym.value() == "char") {
    scan();
  } else if (m_nextsym.type() == TokenType::KEYWORD && m_nextsym.value() == "int") {
    scan();
  } else {
    
  }
}

void Parser::declarator() {
  // TODO
}

void Parser::compoundStatement() {
  // TODO
}
