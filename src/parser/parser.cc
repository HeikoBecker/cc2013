#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"
#include "../lexer/lexer.h"
#include "../lexer/punctuatortype.h"

#define ABORT(X) do {throw std::exception();} while (false)

using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(unique_ptr<Lexer> lexer)
  :  m_nextsym(lexer->getNextToken()), m_lexer(std::move(lexer)) {
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
  return m_nextsym;
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
  cout<<"SCAN :";
  debugOutput();
  m_nextsym = m_lexer->getNextToken();
  return m_nextsym;
}

bool Parser::parse() {
  auto ok = true;

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
  /* this function works even for some operators we don't support */
  if (t.value() == ",") {
    return 0;
  } else if (t.value() == "=") {
    return 1;
  } else if (t.value() == "?" || t.value() == ":") {
    return 2;
  } else if (t.value() == "||") {
    return 3;
  } else if (t.value() == "&&") {
    return 4;
  } else if (t.value() == "|") {
    return 5;
  } else if (t.value() == "^") {
    return 6;
  } else if (t.value() == "&" && !isUnary) {
    return 7;
  } else if (t.value() == "==" || t.value() == "!=") {
    return 8;
  } else if (t.value() == "<" || t.value() == ">") {
    return 9;
  } else if (t.value() == "<<" || t.value() == ">>") {
    return 10;
  } else if (!isUnary && (t.value() == "+" || t.value() == "-")) {
    return 11;
  } else if (   (t.value() == "*" && !isUnary) || t.value() == "/" 
             || t.value() == "%") {
    return 12;
  } else if (   t.value() == "*" 
             || t.value() == "!"
             || t.value() == "&"
             || t.value() == "sizeof"
             || t.value() == "-") {
    /* unary operators */
    return 13;
  } else if (   t.value() == "->"
             || t.value() == "."
             /*TODO: handle () and [] here*/) {
    return 14;
  } else {
    std::cout << t.value() << std::endl;
    return -1;
  }
}

static inline bool isBinaryOperator(Token t) {
  //TODO: remove this nonsense
  if (   t.value() == "||"
      || t.value() == "=="
      || t.value() == "!="
      || t.value() == "<"
      || t.value() == "&&"
      || t.value() == "+"
      || t.value() == "-"
      || t.value() == "*"
      || t.value() == "?"
      || t.value() == ":"
      || t.value() == "="
      ) {
    return true;
  } else {
    return false;
  }
}

static inline bool isRightAssociative(Token t) {
  /* this function currently only works for binary operators */
  if (   t.value() == "="
      || t.value() == "?") {
    return true;
  }
  return false;
}

AstChild Parser::computeAtom() {
  if (testp("(")) { 
    // parse expression in parentheses
    scan();
      auto child = expression(0);
    if (!testp(")")) {
      //unmatched (
      ABORT();
    }
    return child;
  } else if (   m_nextsym.type() == TokenType::IDENTIFIER 
             || m_nextsym.type() == TokenType::CONSTANT) {
    // 'normal ' atom, variable or constant
    // maybe followed by one of ., ->, [], ()
    // TODO: can this follow after a constant?
    bool cont = m_nextsym.type() == TokenType::IDENTIFIER;
    auto var = std::make_shared<Variable>(m_nextsym.value());
    scan();
    auto child = AstChild(var);
    while (cont) {
      if (testp("(")) { //function call operator
        scan();
        // handle function arguments
        auto arguments = std::vector<AstChild> {}; // empty set of arguments
        if (!testp(")")) {
          ABORT();
        }
        child = make_shared<FunctionCall>(child, arguments);
        scan();
      } else if (testp("[")) { //array access(?). TODO: Are expressions supported in []?
        scan();
        auto index = expression(0);
        if (!testp("]")) {
          ABORT();
        }
        scan();
        child = make_shared<UnaryExpression>(PunctuatorType::ARRAY_ACCESS,index);
      } else if (testp("->") || testp(".")) {
        PunctuatorType p = (testp(".")) ? PunctuatorType::MEMBER_ACCESS
                                        : PunctuatorType::ARROW;
        scan();
        if (m_nextsym.type() != TokenType::IDENTIFIER) {
          ABORT();
        }
        auto var = make_shared<Variable>(m_nextsym.value());
        child = make_shared<UnaryExpression>(p,var);
        scan();
      } else {
        cont = !cont;
      }
    }
    return child;
    //expression(1); // TODO: this looks wrong
  } else if (testp("*") || testp("-") || testk("sizeof")) {
    //unary operators: * and -
    auto op = testk("sizeof") ?
              PunctuatorType::SIZEOF :
              ((testp("*"))   ? PunctuatorType::STAR
                              : PunctuatorType::MINUS);
    auto precNext = getPrec(m_nextsym, true);
    scan();
    auto operand = expression(precNext);
    return make_shared<UnaryExpression>(op, operand);
  } else {
    // something went wrong
    // TODO: LATER: return error expression object
    ABORT();
  }
}

AstChild Parser::expression(int minPrecedence = 0) {
  auto expr = computeAtom();
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
  return expr;
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
