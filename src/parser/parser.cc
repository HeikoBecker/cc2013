#include <sstream>
#include <iostream>
#include <exception>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"
#include "../diagnostic.h"
#include "../lexer/lexer.h"
#include "../lexer/punctuatortype.h"
#include "../lexer/keywordtokentype.h"

using namespace Lexing;
using namespace Parsing;


// init parser
Parser::Parser(FILE* f, char const *name)
  :  m_lexer(unique_ptr<Lexer>(new Lexer(f,name))) ,
     m_nextsym(m_lexer->getNextToken()), m_lookahead(m_lexer->getNextToken())
{

}

inline void Parser::reportError(std::string msg = "Parsing error") {
  errorf(m_nextsym->pos(), msg.c_str());
  // TODO: the full featured parser should continue
  throw ParsingException(msg);
};

void Parser::expectedAnyOf() {
  reportError();
}

void Parser::expect(std::string s) {
  if (m_nextsym->value() != s) {
    auto msg = std::string("Expected ");
    msg += s;
    msg += std::string(" but got ");
    msg += m_nextsym->value();
    reportError(msg);
  }
}

void Parser::expect(PunctuatorType puncutator) {
  if (!testp(puncutator)) {
    reportError();
  }
}

void Parser::expect(KeywordType keyword) {
  if (!testk(keyword)) {
    reportError();
  }
}

void Parser::expect(TokenType tokenType) {
  if (getNextType() != tokenType) {
    reportError();
  }
}


void Parser::debugOutput() {
  // print current token
#ifdef DEBUG
  cout<<"SCAN :";
  if (getNextType() == TokenType::END) {
    cout<<"END OF TOKENS REACHED"<<endl;
  } else {
     printToken(*getNextSymbol());
  }
#endif
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
  debugOutput();

  m_nextsym = m_lookahead;
  m_lookahead = m_lexer->getNextToken();
  return m_nextsym;
}

bool Parser::parse() {
  auto ok = true;

  translationUnit();
  expect(TokenType::END);
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
  Type t = typeSpecifier();

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

Type Parser::typeSpecifier() {
  cout<<"Type specifier" <<endl;
  if (testk("struct")) {
    structOrUnionSpecifier();
    // TODO : implement struct
    return BasicType("struct");
  } else {
    BasicType type(m_nextsym->value());
    scan();
    return type;
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
        expect(PunctuatorType::RIGHTPARENTHESIS);
        scan(); // now we've read the closing ")"
        child = make_shared<FunctionCall>(child, arguments);
      } else if (testp(PunctuatorType::LEFTSQBRACKET)) { //array access(?). TODO: Are expressions supported in []?
        scan();
        auto index = expression(0);
        expect(PunctuatorType::RIGHTSQBRACKET);
        scan();
        child = make_shared<UnaryExpression>(PunctuatorType::ARRAY_ACCESS,index);
      } else if (testp(PunctuatorType::ARROW) || testp(PunctuatorType::MEMBER_ACCESS)) {
        PunctuatorType p = (testp(PunctuatorType::MEMBER_ACCESS)) ? PunctuatorType::MEMBER_ACCESS
                                        : PunctuatorType::ARROW;
        scan();
        expect(TokenType::IDENTIFIER);
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
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    // handle postfix-expression
    child = postfixExpression(child);
    return child;
  } else if (   m_nextsym->type() == TokenType::IDENTIFIER 
             || m_nextsym->type() == TokenType::STRINGLITERAL
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
    expectedAnyOf();
    shared_ptr<Expression> errorneous;
    return errorneous;
  }
}

SubExpression Parser::sizeOfType() {
  // TODO: actually construct AST class
  scan(); // read starting parenthesis
  // read type
  typeName();
  expect(PunctuatorType::RIGHTPARENTHESIS);
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
    expect(PunctuatorType::COLON);
    // TODO: do we need to change the value of minPrecedence here?
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

  typeSpecifier();
  if (testp(";")) {
    scan();
  } else {
    declarator();
    expect(PunctuatorType::SEMICOLON);
    scan();
  }
}

/*
struct-or-union-specifier -> "struct" identifier "{" struct-declarations-list "}"
                            | "struct" "{" struct-declarations-list "}"
                                                        | "struct"  identifier
*/
StructType Parser::structOrUnionSpecifier() {
  expect("struct");
  scan();

  if (testType(TokenType::IDENTIFIER)) {
    StructType type(m_nextsym->value());

    scan();
    if (testp("{")) {
      expect(PunctuatorType::LEFTCURLYBRACE);
      scan();
      structDeclarationList();
      expect(PunctuatorType::RIGHTCURLYBRACE);
      scan();
    }

    return type;
  } else if(testp("{")) {
    scan();
    structDeclarationList();
    expect(PunctuatorType::RIGHTCURLYBRACE);
    scan();
  } else {
    expectedAnyOf();
  }

  return StructType();
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
     expect(PunctuatorType::SEMICOLON);
     scan();
    }
  } else if(testk("_Static_assert")) {
    staticAssert();
  } else {
    expectedAnyOf();
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
    constantExpression();
  } else {
    declarator();
    if (testp(":")) {
      scan();
      constantExpression();
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
abstract-declarator ->  pointer
                      | pointer direct-abstract-declarator
                                            | direct-abstract-declarator

                                            direct-abstract-declarator ->  "(" abstract-declarator ")" direct-abstract-declarator_help

                                            direct-abstract-declarator_help -> "(" parameter-type-list ")" direct-abstract-declarator_help
                                                                         |   "(" ")" direct-abstract-declarator_help
                                                                                                      | EPSILON
*/
void Parser::abstractDeclarator() {
  while(testp("*")) {
    scan();
  } 

  if (testp("(")) {
    directAbstractDeclarator();
  }
}

/*
direct-abstract-declarator ->  "(" abstract-declarator ")" direct-abstract-declarator_help

                                            direct-abstract-declarator_help -> "(" parameter-type-list ")" direct-abstract-declarator_help
                                                                         |   "(" ")" direct-abstract-declarator_help
                                                                                                      | EPSILON
*/
void Parser::directAbstractDeclarator() {
  expect("(");
  scan();
  abstractDeclarator();
  expect(")");
  scan();

  if(testp("(")) {
    directAbstractDeclaratorHelp();
  }
}

void Parser::directAbstractDeclaratorHelp() {
  expect("(");
  scan();
  
  if (testp(")")) {
    scan();
    if(testp("(")) {
      directAbstractDeclaratorHelp();
    }
  } else {
    parameterList();
    expect(")");
    scan();

    if(testp("(")) {
      directAbstractDeclaratorHelp();
    }
  }
}

/*
 *
 parameter-declaration ->   declaration-specifiers declarator
 | declarations-specifiers abstract-declarator
 | declarations-specifiers
*/
void Parser::parameterDeclaration() {
  typeSpecifier();
  if (testp(PunctuatorType::COMMA) || testp(PunctuatorType::RIGHTPARENTHESIS)) {
    return;
  } else {
    declarator();
    // TODO distinguish abstract-declarator
  }
}
/*
identifier-list ->  identifier
                  | identifier "," identifier-list
*/
void Parser::identifierList() {
  expect(TokenType::IDENTIFIER);
  scan();

  while (testp(",")) {
    scan();

    expect(TokenType::IDENTIFIER);
    scan();
  }
}

void Parser::typeName() {
  specifierQualifierList();

  if (!testp(")")) {
    abstractDeclarator();
  }
}

/*
declarator ->   pointer direct-declarator
              | direct-declarator
*/
void Parser::declarator() {

  int counter = 0;

  if(test(TokenType::PUNCTUATOR,"*")) {
    counter++;
    pointer();
  }

  Pointer pointer(counter);

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
      expect(PunctuatorType::RIGHTPARENTHESIS);
      scan();

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
CompoundStatement Parser::compoundStatement() {

  if (testp(PunctuatorType::LEFTCURLYBRACE)) {
    scan();

    if (testp(PunctuatorType::RIGHTCURLYBRACE)) {
      scan();
      return CompoundStatement();
    } else {
      blockItemList();
      if(testp(PunctuatorType::RIGHTCURLYBRACE)) {
        scan();

        // TODO fill CompundStatement with BlockList
        return CompoundStatement();
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
      expectedAnyOf();
    }
  }

}

/*
labeled-statement -> identifier : statement
*/
void Parser::labeledStatement() {
  if(testType(TokenType::IDENTIFIER)) {
    scan();
    expect(PunctuatorType::COLON);
    scan();
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
    expect(PunctuatorType::LEFTPARENTHESIS);
    scan();
    expression();
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    statement();
  } else if (testk(KeywordType::DO)) {
    scan();
    statement();
    expect("while");
    scan();
    expect("(");
    scan();
    expression();
    expect(")");
    scan();
    expect(";");
    scan();

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

    expect("(");
    scan();
    expression();
    expect(")");
    scan();
    statement();

    if(testk(KeywordType::ELSE)) {
      scan();
      // TODO
      // return SelectionStatement(ex, if, else);
      statement();
    } else {
      // TODO: return SelectionStatement(ex, if);
    }
  } else {
    throw "selectionStatement: no match";
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
SubJumpStatement Parser::jumpStatement() {
  if (testk(KeywordType::GOTO)) {
    scan();
    if(testType(TokenType::IDENTIFIER)) {

      SubJumpStatement gotoStatement = make_shared<GotoStatement>(m_nextsym->value());
      scan();

      expect(";");
      scan();

      return gotoStatement;
    } else {
      throw "jump-statement: identifier expected";
    }
  } else if (testk(KeywordType::CONTINUE)) {
    scan();
    expect(";");
    scan();

    return make_shared<ContinueStatement>();
  } else if (testk(KeywordType::BREAK)) {
    scan();
    expect(";");
    scan();
    return make_shared<BreakStatement>();
  } else if (testk(KeywordType::RETURN)){
    scan();

    if(testp(PunctuatorType::SEMICOLON)) {
      scan();

      return make_shared<ReturnStatement>();
    } else {
      SubExpression sub = expression();
      expect(";");
      scan();

      return make_shared<ReturnStatement>(sub);
    }
  } else {
    throw "jump-statement : unexpected token";
  }
}

void Parser::staticAssert() {
  expect("_Static_assert");
  scan();
  expect("(");
  scan();

  constantExpression();

  expect(",");
  scan();

  expect(TokenType::STRINGLITERAL);
  scan();

  expect(")");
  scan();

  expect(";");
  scan();
}

void Parser::constantExpression() {
  // TODO: use constant expression
  expression();
}

