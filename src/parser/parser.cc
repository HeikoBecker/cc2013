#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"
#include "../lexer/lexer.h"

#define ABORT(X) do {throw std::exception();} while (false)

using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(vector<Token> tokens) : m_tokenList(std::move(tokens)) , m_nextsym(m_tokenList.front()) {
  // should you really copy?
  // or is a reference enough
  posTokenList = -1;
}

void Parser::debugOutput() {
  // print current token
  printToken(getNextSymbol());
}

Token Parser::getNextSymbol() {
  return m_tokenList[posTokenList];
}

TokenType Parser::getNextType() {
  return getNextSymbol().type();
}

string Parser::getNextValue() {
  return getNextSymbol().value(); 
}

Token Parser::scan() {
  posTokenList++;
  // TODO : throw exception when scanning after list
  return m_tokenList[posTokenList];
}

bool Parser::parse() {
  auto ok = true;

  /*This is the entry point*/
  scan();

  debugOutput();

  scan();
  
  debugOutput();

  translationUnit();
  if (m_nextsym.type() != TokenType::END) {
    ABORT();
  }
  return ok;
}

void Parser::translationUnit() {
  // TODO: handle more than one externalDeclaration
  externalDeclaration();
}

void Parser::externalDeclaration() {
  // TODO : also handle declaration 
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
    // TODO: support structs
  }
}

void Parser::declarator() {
  // TODO
}

void Parser::compoundStatement() {
  // TODO
}
