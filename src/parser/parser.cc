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
  if (getNextType() == TokenType::END) {
    cout<<"END OF TOKENS REACHED"<<endl;
  } else {
     printToken(getNextSymbol());
  }
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

bool Parser::testType(TokenType type) {
  return getNextType() == type;
}

bool Parser::testValue(string value) {
  return getNextValue() == value;
}

bool Parser::test(TokenType type, string val) {
  return testType(type) && testValue(val);
}

bool Parser::testp(string val) {
  return test(TokenType::PUNCTUATOR, val);
}

bool Parser::testk(string val) {
  return test(TokenType::KEYWORD, val);
}

Token Parser::scan() {
  posTokenList++;
  // TODO : throw exception when scanning after list
  
  cout<<"SCAN :";
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
  while (!testType(TokenType::END)) {
    externalDeclaration();
  }
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
  cout<<"compoundStatement();"<<endl;
  compoundStatement();
}

void Parser::declarationSpecifiers() {
  typeSpecifier();
}


void Parser::typeSpecifier() {
  if (test(TokenType::KEYWORD, "void")) {
    scan();
  } else if (test(TokenType::KEYWORD, "char")) {
    scan();
  } else if (test(TokenType::KEYWORD, "int")) {
    scan();
  } else if (test(TokenType::KEYWORD, "struct")) {
    structOrUnionSpecifier();
  } else {
    throw "typespecifier : no matching";
  }
}

static inline int getPrec(Token t, bool isUnary = false) {
  if (t.value() == "=") {
    return 0;
  } else if (t.value() == "?" || t.value() == ":") {
    return 1;
  } else if (   t.value() == "*" || t.value() == "&" 
             || t.value() == "sizeof") {
    return 2;
  } else if (t.value() == "||") {
    return 0;
  } else if (t.value() == "&&") {
    return 1;
  } else if (t.value() == "+" || t.value() == "-") {
    return 2;
  } else if (t.value() == "*") {
    if (isUnary) {
      return 42; // TODO: recheck
    }
    return 3;
  } else {
    ABORT();
    return 0;
  }
}

static inline bool isBinaryOperator(Token t) {
  //TODO: remove this nonsense
  if (t.value() == "dutriaenfqu") {
    return false;
  }
  return true;
}

static inline bool isRightAssociative(Token t) {
  //TODO
  if (t.value() == "=") {
    return true; // TODO: check
  }
  return false;
}

void Parser::computeAtom() {
  if (testp("(")) { 
    // parse expression in parentheses
    scan();
      expression(0);
    if (!testp(")")) {
      //unmatched (
      ABORT();
    }
  } else if (   m_nextsym.type() == TokenType::IDENTIFIER 
             || m_nextsym.type() == TokenType::CONSTANT) {
    // 'normal ' atom, variable or constant
    scan();
    expression(1);
  } else if (testp("*") || testp("-")) {
    //unary operators: * and -
    auto precNext = getPrec(m_nextsym, true);
    scan();
    expression(precNext);
  } else {
    // something went wrong
    // TODO: LATER: return error expression object
  }
}

void Parser::expression(int minPrecedence = 0) {
  computeAtom();
  // handle ternary operator
  if (testp("?")) {
    scan();
    expression(/*TODO: which precedence should this be*/100);
    // m_nextsym now has to be a : -- else this wouldn't be a valid ternary
    // operator
    if (!testp(":")) {
      ABORT(); //TODO
    } else {
      // do we need to change the value of minPrecedence here?
    }
  }  
  while (isBinaryOperator(m_nextsym) && getPrec(m_nextsym) >= minPrecedence) {
    auto precNext = (isRightAssociative(m_nextsym))
                    ? getPrec(m_nextsym)
                    : getPrec(m_nextsym) + 1;
    scan();
    expression(precNext);
  }
}



void Parser::declaration() {
  // TODO
}

void Parser::initDeclaratorList() {
  // TODO
}

void Parser::initDeclarator() {
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
  while(testp("*")) {
    scan();
  }
}


/*

parameter-list  ->   parameter-declaration
                   | parameter-list "," parameter-declaration
*/
void Parser::parameterList() {
  parameterDeclaration();

  while(testp(",")) {
    scan();
    parameterDeclaration();
  }
}


/*
 *
 parameter-declaration ->   declaration-specifiers declarator
 | declarations-specifiers abstract-declarator
 | declarations-specifiers
*/

void Parser::parameterDeclaration() {
  declarationSpecifiers();
  if (testp(",") || testp(")")) {
    return;
  } else {
    declarator();
    // TODO distinguish abstract-declarator
  }
}

void Parser::identifierList() {
  // TODO
}

void Parser::typeName() {
  // TODO
}

/*
declarator ->   pointer direct-declarator
              | direct-declarator
*/
void Parser::declarator() {

  if(test(TokenType::PUNCTUATOR,"*")) {
    pointer();
  }

  directDeclarator();
}

/*
direct-declarator -> identifier direct-declarator_help
                   | "(" declarator ")" direct-declarator_help

*/
void Parser::directDeclarator() {
  if (testType(TokenType::IDENTIFIER)) {
    scan();

    if(testp("(")) {
      directDeclaratorHelp();
    }

  } else if (testp("(")) {
    scan();
    declarator();

    if(testp(")")) {
      scan();
    } else {
      throw "direct-declarator : expected ')'";
    }

    if(testp("(")) {
      directDeclaratorHelp();
    }

  } else {
    throw "error in direct Declarator";
  }

}

/*
direct-declarator_help -> "(" parameter-list ")" direct-declarator_help
             | "(" identifier-list ")" direct-declarator_help
                          |  "(" ")" direct-declarator_help
                                       | EPSILON
*/
void Parser::directDeclaratorHelp() {
  if (testp("(")) {
    scan();

    // 1. option
    if(testp(")")) {
      scan();
      
      if(testp("(")) {
        directDeclaratorHelp();
      }

      return;
    } else if (testTypeSpecifier()) { // parameter-list
      parameterList();
      readP(")");

      if(testp("(")) {
        directDeclaratorHelp();
      }
    } 

    // TODO : parameter-list
    // TODO: identifier-list
  } else {
    throw "direct-declatror_help : '(' expected";
  }

}

void Parser::directOrAbstractDeclarator(bool isDirect) {
  if (isDirect) {
    // so we have no compile error 
    directDeclarator();
  } else {
    throw "astract declarator is not implemented yet";
  }
}

/**
compound-statement -> "{" block-item-list "}"
                     |  "{" "}"
*/
void Parser::compoundStatement() {
  if (testp("{")) {
    scan();

    if (testp("}")) {
      scan();
      return;
    } else {
      blockItemList();
      if(testp("}")) {
        scan();
        return;
      } else {
        throw "compoundStatement: '}' expected";
      }
    }

  } else {
    throw "compoundStatement: '{' expected";
  }
}

/*
block-item-list ->  block-item
                  | block-item block-item-list
*/
void Parser::blockItemList() {
  while(!testp("}")) {
    blockItem();
  }
}

bool Parser::testTypeSpecifier() {
  return testType(TokenType::KEYWORD) && (
    testValue("void") || testValue("int") ||
    testValue("char") || testValue("struct")
  );
}


/*
block-item ->   declaration
              | statement
*/
void Parser::blockItem() {
  // TODO : handle static asser declaration
  if (testTypeSpecifier()) {
    declaration();
  } else {
    statement();
  }
}

/**
statement ->   
    labeled-statement
  | compound-statement
  | expression-statement 
  | selection-statement
  | iteration-statement
  | jump-statement
*/
void Parser::statement() {
  if(testk("goto") || testk("continue") || testk("break") || testk("return")) {
    // jump-statement
    jumpStatement();
  } else if(testp("{")) {
    compoundStatement();
  } else if(testk("if")) {
    selectionStatement();
  } else if(testk("while") || testk("do")) {
    iterationStatement();
  } else if(testType(TokenType::IDENTIFIER)) {
    // TODO: is it clear that it is not an expression
    labeledStatement();
  } else {
    expression();
  }
}

/*
labeled-statement -> identifier : statement
*/
void Parser::labeledStatement() {
  if(testType(TokenType::IDENTIFIER)) {
    scan();
    readP(":");
    statement();
  } else {
    throw "labeled-statement : identifier expected";
  }
}

/*
iteration-statement ->  "while" "(" expression ")" statement
                      | "do" statement "while" "(" expression ")" ";"

*/
void Parser::iterationStatement() {
  if(testk("while")) {
    scan();
    readP("(");
    expression();
    readP(")");
    statement();
  } else if (testk("do")) {
    scan();
    statement();
    readK("while");
    readP("(");
    expression();
    readP(")");
    readP(";");

  } else {
    throw "iteration-statement : no match found";
  }
  
}


/*
selection-statement ->   "if" "(" expression ")" statement
   | "if" "(" expression ")" statement "else" statement
*/

void Parser::selectionStatement() {
  if (testk("if")) {
    scan();

    readP("(");
    expression();
    readP(")");
    statement();

    if(testk("else")) {
      scan();
      statement();
    }
  } else {
    throw "selectionStatement: no match";
  }
}

void Parser::readP(string value) {
  if(testp(value)) {
    scan();
  } else {
    throw "error: "+value+" expected";
  }
}

void Parser::readK(string value) {
  if(testk(value)) {
    scan();
  } else {
    throw "error: "+value+" expected";
  }
}


void Parser::readSemicolon(string funcName) {
  if(testp(";")) {
    scan();
  } else {
    throw funcName+" expected";
  }
}

/*
jump-statement ->  
  "goto" identifier ";"
  "continue" ";"
  "break" ";"
  "return" expression ";"
  "return" ";"
*/
void Parser::jumpStatement() {
  if (testk("goto")) {
    scan();
    if(testType(TokenType::IDENTIFIER)) {
      scan();

      readSemicolon("jump-statement");
      
    } else {
      throw "jump-statement: identifier expected";
    }
  } else if (testk("continue")) {
    scan();
    readSemicolon("jump-statement");
  } else if (testk("break")) {
    scan();
    readSemicolon("jump-statement");
  } else if (testk("return")){
    scan();

    if(testp(";")) {
      scan();
    } else {
      expression();
      readSemicolon("jump-statement");
    }

  } else {
    throw "jump-statement : unexpected token";
  }
}
