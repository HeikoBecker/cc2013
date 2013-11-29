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
  
  cout<<"SCAN "<<endl;
  debugOutput();
  return m_tokenList[posTokenList];
}

bool Parser::parse() {
  auto ok = true;

  /*This is the entry point*/
  scan();


  translationUnit();
  if (getNextType() != TokenType::END) {
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
  cout<<"first symbol"<<endl;
  declarationSpecifiers();
  cout<<"declarator"<<endl;
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

void Parser::primaryExpression() {
  // TODO
}

void Parser::postfixExpression() {
  // TODO
}

void Parser::argumentExpressionList() {
  // TODO
}

void Parser::unaryExpression() {
  // TODO
}

void Parser::additiveExpression() {
  // TODO
}

void Parser::multiplicativeExpression() {
  // TODO
}

void Parser::castExpression() {
  // TODO
}

void Parser::declaration() {
  // TODO
}

void Parser::initDeclaratorList() {
  // TODO
}

void Parser::initDeclarator() {
  // TODO
}

void Parser::structOrUnion() {
  // TODO
}

void Parser::structOrUnionSpecifier() {
  // TODO
}

void Parser::structDeclarationList() {
  // TODO
}

void Parser::structDeclaration() {
  // TODO
}

void Parser::specifierQualifierList() {
  // TODO
}

void Parser::structDeclaratorList() {
  // TODO
}

void Parser::structDeclarator() {
  // TODO
}

void Parser::enumSpecifier() {
  // TODO
}

void Parser::enumeratorList() {
  // TODO
}

void Parser::enumerator() {
  // TODO
}

void Parser::pointer() {
  // TODO
}

void Parser::parameterTypeList() {
  // TODO
}

void Parser::parameterList() {
  // TODO
}

void Parser::parameterDeclaration() {
  // TODO
}

void Parser::identifierList() {
  // TODO
}

void Parser::typeName() {
  // TODO
}

void Parser::declarator() {
  // TODO
}

void Parser::directOrAbstractDeclarator(bool isDirect) {
  if (isDirect) {
    // so we have no compile error 
  }
  // TODO
}

void Parser::compoundStatement() {
  // TODO
}
