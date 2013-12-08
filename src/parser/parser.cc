#include <sstream>
#include <iostream>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"
#include "../lexer/lexer.h"
#include "../lexer/punctuatortype.h"
#include "../lexer/keywordtokentype.h"

#define ABORT(X) do {throw std::exception();} while (false)

using namespace Lexing;
using namespace Parsing;

// init parser
Parser::Parser(FILE* f, char const *name)
  :  m_lexer(unique_ptr<Lexer>(new Lexer(f,name))) ,
     m_nextsym(m_lexer->getNextToken()), m_lookahead(m_lexer->getNextToken())
{

}

void Parser::debugOutput() {
  // print current token
  if (getNextType() == TokenType::END) {
    cout<<"END OF TOKENS REACHED"<<endl;
  } else {
     printToken(*getNextSymbol());
  }
}

std::shared_ptr<Token> Parser::getNextSymbol() {
  return m_nextsym;
}

TokenType Parser::getNextType() {
  return getNextSymbol()->type();
}

string Parser::getNextValue() {
  return getNextSymbol()->value(); 
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

bool Parser::testLookAheadP(string val) {
  return m_lookahead->value() == val && 
         m_lookahead->type() == TokenType::PUNCTUATOR;
}

bool Parser::testp(PunctuatorType puncutator) {
  if (testType(TokenType::PUNCTUATOR)) {
    return     std::static_pointer_cast<PunctuatorToken>(m_nextsym)->punctype() 
            == puncutator;
  } else {
    return false;
  }
}

bool Parser::testk(string val) {
  return test(TokenType::KEYWORD, val);
}

bool Parser::testk(KeywordType keyword) {
  if (testType(TokenType::KEYWORD)) {
    return     std::static_pointer_cast<KeywordToken>(m_nextsym)->keywordtype() 
            == keyword;
  } else {
    return false;
  }
}

std::shared_ptr<Token> Parser::scan() {
  cout<<"SCAN :";
  debugOutput();

  m_nextsym = m_lookahead;
  m_lookahead = m_lexer->getNextToken();
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

  if (testk("_Static_assert")) {
    staticAssert();
    return ;
  }
  // functionDefintion or declaration ?
  declarationSpecifiers();

  if (testp(";")) {
    // it was a declaration
    scan();
    return ;
  }
  declarator();

  if (testp(";")) {
    scan();
    // it was a declaration()
    return ;
  }

  // it is a functionDefition! 
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
  } else if (t.value() == "?") {
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
  } else if ( t.value() == ":") {
    return 15;
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

SubExpression Parser::postfixExpression(SubExpression child) {
    auto cont = true;
    while (cont) {
      if (testp(PunctuatorType::LEFTPARENTHESIS)) { //function call operator
        scan();
        // handle function arguments
        auto arguments = std::vector<SubExpression> {}; // empty set of arguments
        if (!testp(PunctuatorType::RIGHTPARENTHESIS)) {
          auto multipleArguments = false;
          do {
            if (multipleArguments) {
              scan(); // read the , separating the arguments
            } else {
              /* This happens only when we read the first argument
               * If there is only one argument, the value of multipleArguments
               * is not important; else it ensures that we read the ,
               * */
              multipleArguments = true;
            }
            arguments.push_back(expression(0));
          } while (testp(PunctuatorType::COMMA));
        }
        if (testp(PunctuatorType::RIGHTPARENTHESIS)) {
          scan(); // now we've read the closing ")"
        } else {
          ABORT();
        }
        child = make_shared<FunctionCall>(child, arguments);
      } else if (testp(PunctuatorType::LEFTSQBRACKET)) { //array access(?). TODO: Are expressions supported in []?
        scan();
        auto index = expression(0);
        if (!testp(PunctuatorType::RIGHTSQBRACKET)) {
          ABORT();
        }
        scan();
        child = make_shared<UnaryExpression>(PunctuatorType::ARRAY_ACCESS,index);
      } else if (testp(PunctuatorType::ARROW) || testp(PunctuatorType::MEMBER_ACCESS)) {
        PunctuatorType p = (testp(PunctuatorType::MEMBER_ACCESS)) ? PunctuatorType::MEMBER_ACCESS
                                        : PunctuatorType::ARROW;
        scan();
        if (m_nextsym->type() != TokenType::IDENTIFIER) {
          ABORT();
        }
        auto var = make_shared<VariableUsage>(m_nextsym->value());
        child = make_shared<UnaryExpression>(p,var);
        scan();
      } else {
        cont = !cont;
      }
    }
    return child;
}

SubExpression Parser::computeAtom() {
  if (testp(PunctuatorType::LEFTPARENTHESIS)) { 
    // parse expression in parentheses
    scan();
    auto child = expression(0);
    if (!testp(PunctuatorType::RIGHTPARENTHESIS)) {
      //unmatched (
      ABORT();
    }
    scan();
    // handle postfix-expression
    child = postfixExpression(child);
    return child;
  } else if (   m_nextsym->type() == TokenType::IDENTIFIER 
             || m_nextsym->type() == TokenType::CONSTANT) {
    // 'normal ' atom, variable or constant
    // maybe followed by one of ., ->, [], ()
    auto var = std::make_shared<VariableUsage>(m_nextsym->value());
    auto cont = m_nextsym->type() == TokenType::IDENTIFIER;
    scan();
    auto child = SubExpression(var);
    // handle postfix-expression
    if (cont) {
      child = postfixExpression(child);
    }
    return child;
  } else if (testp(   PunctuatorType::STAR) 
                   || testp(PunctuatorType::MINUS) 
                   || testk(KeywordType::SIZEOF)
                   || testp(PunctuatorType::NOT)
                   || testp(PunctuatorType::AMPERSAND)) {
    //unary operators: * and -
    auto op = testk(KeywordType::SIZEOF) ? PunctuatorType::SIZEOF 
                                         : static_pointer_cast<PunctuatorToken>(
                                             m_nextsym)->punctype();
    auto precNext = getPrec(*m_nextsym, true);
    scan();
    auto operand = SubExpression{};
    // sizeof needs special care if it is applied to a type
    // TODO: get rid of ridiculous if expression
    if (    PunctuatorType::SIZEOF == op
         && testp(PunctuatorType::LEFTPARENTHESIS)
         && m_lookahead->type() == TokenType::KEYWORD
         && (   m_lookahead->value() == "char"
             || m_lookahead->value() == "int"
             || m_lookahead->value() == "void"
             || m_lookahead->value() == "struct")) {
      operand = sizeOfType(); 
    } else {
      operand = expression(precNext);
    }
    return make_shared<UnaryExpression>(op, operand);
  } else {
    // something went wrong
    // TODO: LATER: return error expression object
    std::cerr << m_nextsym->value() ;
    ABORT();
  }
}

SubExpression Parser::sizeOfType() {
  // TODO: actually construct AST class
  scan(); // read starting parenthesis
  // read type
  typeName();
  if (!testp(PunctuatorType::RIGHTPARENTHESIS)) {
    ABORT();
  }
  scan();  // read closing parenthesis
  return SubExpression{};
}

SubExpression Parser::expression(int minPrecedence = 0) {
  auto expr = computeAtom();
  // handle ternary operator
  auto isTernary = false;
  SubExpression ternaryHelper;
  if (testp(PunctuatorType::QMARK)) {
    scan();
    isTernary = true;
    ternaryHelper = expression(/*TODO: which precedence should this be*/0);
    // m_nextsym now has to be a : -- else this wouldn't be a valid ternary
    // operator
    if (!testp(PunctuatorType::COLON)) {
      ABORT(); //TODO
    } else {
      // do we need to change the value of minPrecedence here?
    }
  }  
  while (  (isBinaryOperator(*m_nextsym) && getPrec(*m_nextsym) >= minPrecedence)
         || isTernary) {
    int precNext;
    if (isTernary) {
      precNext = 2;
      isTernary = false;
    } else {
      precNext = (isRightAssociative(*m_nextsym))
                 ? getPrec(*m_nextsym)
                 : getPrec(*m_nextsym) + 1;
    }
    scan(); // this will either read the binary operator or ":" if we're parsing the ternary operator
    auto rhs = expression(precNext);
    if (!isTernary) {
      expr = make_shared<BinaryExpression>(expr, rhs, PunctuatorType::ILLEGAL);
    } else {
      expr = make_shared<TernaryExpression>(expr, ternaryHelper, rhs);
    }
  }
  return expr;
}



void Parser::declaration() {
  if (testk("_Static_assert")) {
    staticAssert();
    return ;
  }

  declarationSpecifiers();
  if (testp(";")) {
    scan();
  } else {
    declarator();
    readP(";");
  }
}

void Parser::initDeclaratorList() {
  // TODO
}

void Parser::initDeclarator() {
}

/*
struct-or-union-specifier -> "struct" identifier "{" struct-declarations-list "}"
                            | "struct" "{" struct-declarations-list "}"
                                                        | "struct"  identifier
*/
void Parser::structOrUnionSpecifier() {
  readK("struct");

  if (testType(TokenType::IDENTIFIER)) {
    scan();
    if (testp("{")) {
      readP("{");
      structDeclarationList();
      readP("}");
    }
  } else if(testp("{")) {
    scan();
    structDeclarationList();
    readP("}");
  } else {
    ABORT();
  }
}

void Parser::structDeclarationList() {
  structDeclaration();
  while (!testp("}")) {
    structDeclaration();
  } 
}

void Parser::structDeclaration() {
  if(testTypeSpecifier()) {
    specifierQualifierList();
    if (testp(";")) {
      scan();
    } else {
     structDeclaratorList();
     readP(";");
    }
  } else if(testk("_Static_assert")) {
    staticAssert();
  } else {
    ABORT();
  }
}

/*
specifier-qualifier-list ->   type-specifier
*/
void Parser::specifierQualifierList() {
  typeSpecifier();
}

void Parser::structDeclaratorList() {
  structDeclarator();

  while(testp(",")) {
    scan();
    structDeclarator(); 
  }
}

void Parser::structDeclarator() {
  if (testp(":")) {
    scan();
    // TODO : replace with constant-Expression
    expression();
  } else {
    declarator();
    if (testp(":")) {
      scan();
      // TODO: replace with constant-expression
      expression();
    }
  }
}


void Parser::pointer() {
  while(testp(PunctuatorType::STAR)) {
    scan();
  }
}

/*
parameter-list  ->   parameter-declaration
                   | parameter-list "," parameter-declaration
*/
void Parser::parameterList() {
  parameterDeclaration();

  while(testp(PunctuatorType::COMMA)) {
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
  if (testp(PunctuatorType::COMMA) || testp(PunctuatorType::RIGHTPARENTHESIS)) {
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
  //TODO: what's  below is wrong, or rather not everything that's needed
  specifierQualifierList();
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

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
      directDeclaratorHelp();
    }

  } else if (testp(PunctuatorType::LEFTPARENTHESIS)) {
    scan();
    declarator();

    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();
    } else {
      throw "direct-declarator : expected ')'";
    }

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
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
  if (testp(PunctuatorType::LEFTPARENTHESIS)) {
    scan();

    // 1. option
    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();
      
      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        directDeclaratorHelp();
      }

      return;
    } else if (testTypeSpecifier()) { // parameter-list
      parameterList();
      readP(")");

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
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

  if (testp(PunctuatorType::LEFTCURLYBRACE)) {
    scan();

    if (testp(PunctuatorType::RIGHTCURLYBRACE)) {
      scan();
      return;
    } else {
      blockItemList();
      if(testp(PunctuatorType::RIGHTCURLYBRACE)) {
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
  while(!testp(PunctuatorType::RIGHTCURLYBRACE)) {
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
  if (testk("_Static_assert")) {
    staticAssert();
  } else if (testTypeSpecifier()) {
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
  if(testk(   KeywordType::GOTO) 
           || testk(KeywordType::CONTINUE) 
           || testk(KeywordType::BREAK) 
           || testk(KeywordType::RETURN)) {
    // jump-statement
    jumpStatement();
  } else if(testp(PunctuatorType::LEFTCURLYBRACE)) {
    compoundStatement();
  } else if(testk(KeywordType::IF)) {
    selectionStatement();
  } else if(testk(KeywordType::WHILE) || testk(KeywordType::DO)) {
    iterationStatement();
  } else if(testType(TokenType::IDENTIFIER)) {
    if (testLookAheadP(":")) {
      labeledStatement();
    } else {
      expressionStatement();
    }
  } else {
    expressionStatement();
  }
}

/*
 expression-statement -> ";" | expression ";"
 */
void Parser::expressionStatement() {
  if (testp(PunctuatorType::SEMICOLON)) {
    scan();
    return;
  } else {
    expression();
    if (testp(PunctuatorType::SEMICOLON)) {
      scan();
      return;
    } else {
      ABORT();
    }
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
  if(testk(KeywordType::WHILE)) {
    scan();
    readP("(");
    expression();
    readP(")");
    statement();
  } else if (testk(KeywordType::DO)) {
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
  if (testk(KeywordType::IF)) {
    scan();

    readP("(");
    expression();
    readP(")");
    statement();

    if(testk(KeywordType::ELSE)) {
      scan();
      statement();
    }
  } else {
    throw "selectionStatement: no match";
  }
}

//TODO: port to new readP method
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
  if(testp(PunctuatorType::SEMICOLON)) {
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
  if (testk(KeywordType::GOTO)) {
    scan();
    if(testType(TokenType::IDENTIFIER)) {
      scan();

      readSemicolon("jump-statement");
      
    } else {
      throw "jump-statement: identifier expected";
    }
  } else if (testk(KeywordType::CONTINUE)) {
    scan();
    readSemicolon("jump-statement");
  } else if (testk(KeywordType::BREAK)) {
    scan();
    readSemicolon("jump-statement");
  } else if (testk(KeywordType::RETURN)){
    scan();

    if(testp(PunctuatorType::SEMICOLON)) {
      scan();
    } else {
      expression();
      readSemicolon("jump-statement");
    }

  } else {
    throw "jump-statement : unexpected token";
  }
}

void Parser::staticAssert() {
  readK("_Static_assert");
  readP("(");

  // TODO constant-expression instead of expression
  expression();

  readP(",");

  if(testType(TokenType::STRINGLITERAL)) {
    scan();
  } else {
    ABORT();
  }

  readP(")");
  readP(";");
}

