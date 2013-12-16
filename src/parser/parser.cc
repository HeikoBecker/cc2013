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

[[noreturn]] inline void Parser::reportError(std::string msg = "Parsing error") {
  errorf(m_nextsym->pos(), msg.c_str());
  // TODO: the full featured parser should continue
  throw ParsingException(msg);
}

[[noreturn]] void Parser::expectedAnyOf(std::string msg = "Parsing error") {
  reportError(msg);
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

bool Parser::testTypeSpecifier() {
  return testType(TokenType::KEYWORD) && (
    testValue("void") || testValue("int") ||
    testValue("char") || testValue("struct")
  );
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

AstRoot Parser::parse() {
  auto tu = translationUnit();
  expect(TokenType::END);
  return tu;
}

TUNode Parser::translationUnit() {
  std::vector<ExternalDeclarationNode> externalDeclarations {};
  while (!testType(TokenType::END)) {
     externalDeclarations.push_back(externalDeclaration());
  }
  return make_shared<TranslationUnit>(externalDeclarations);
}

ExternalDeclarationNode Parser::externalDeclaration() {

  // functionDefintion or declaration ?
  auto type = typeSpecifier();

  if (testp(";")) {
    // it was a declaration
    scan();
    return make_shared<ExternalDeclaration>(type);
  }
  auto decl = declarator();

  if (testp(";")) {
    scan();
    // it was a declaration()
    return make_shared<ExternalDeclaration>(type, decl);
  }

  // it is a functionDefition! 
  auto compStat = compoundStatement();
  return make_shared<ExternalDeclaration>(type, decl, compStat);
}

TypeNode Parser::typeSpecifier() {
  if (testk("struct")) {
    return structOrUnionSpecifier();
  } else {
    auto type = std::make_shared<BasicType>(m_nextsym->value());
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
              expect(PunctuatorType::COMMA);
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
        child = make_shared<BinaryExpression>(child,index, PunctuatorType::ARRAY_ACCESS);
      } else if (testp(PunctuatorType::ARROW) || testp(PunctuatorType::MEMBER_ACCESS)) { // member access
        PunctuatorType p = (testp(PunctuatorType::MEMBER_ACCESS)) ? PunctuatorType::MEMBER_ACCESS
                                        : PunctuatorType::ARROW;
        scan();
        expect(TokenType::IDENTIFIER);
        auto var = make_shared<VariableUsage>(m_nextsym->value());
        child = make_shared<BinaryExpression>(child, var, p);
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
  auto type = typeName();
  expect(PunctuatorType::RIGHTPARENTHESIS);
  scan();  // read closing parenthesis
  return make_shared<SizeOfExpression>(type);
}

SubExpression Parser::expression(int minPrecedence = 0) {
  auto expr = computeAtom();
  // handle ternary operator
  auto isTernary = false;
  // TODO: if we don't use Booleans isTernary and wasTernary can be merged
  auto wasTernary = false;
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
    auto punctype = static_pointer_cast<PunctuatorToken>(m_nextsym)->punctype();
    int precNext;
    if (isTernary) {
      precNext = 2;
      isTernary = false;
      wasTernary = true;
    } else {
      precNext = (isRightAssociative(*m_nextsym))
                 ? getPrec(*m_nextsym)
                 : getPrec(*m_nextsym) + 1;
    }
    scan(); // this will either read the binary operator or ":" if we're parsing the ternary operator
    auto rhs = expression(precNext);
    if (!wasTernary) {
      expr = make_shared<BinaryExpression>(expr, rhs, punctype);
    } else {
      expr = make_shared<TernaryExpression>(expr, ternaryHelper, rhs);
    }
  }
  return expr;
}

DeclarationNode Parser::declaration() {

  auto type = typeSpecifier();
  if (testp(";")) {
    scan();
    return std::make_shared<Declaration>(type);
  } else {
    auto decl = declarator();
    expect(PunctuatorType::SEMICOLON);
    scan();
    return std::make_shared<Declaration>(type, decl);
  }
}

/*
struct-or-union-specifier -> "struct" identifier "{" struct-declarations-list "}"
                            | "struct" "{" struct-declarations-list "}"
                                                        | "struct"  identifier
*/
StructNode Parser::structOrUnionSpecifier() {
  expect("struct");
  scan();

  if (testType(TokenType::IDENTIFIER)) {
    auto name = m_nextsym->value();

    scan();
    if (testp("{")) {
      expect(PunctuatorType::LEFTCURLYBRACE);
      scan();
      auto structDecLst = structDeclarationList();
      expect(PunctuatorType::RIGHTCURLYBRACE);
      scan();
      return make_shared<StructType>(name, structDecLst);
    }

    return make_shared<StructType>(name);
  } else if(testp("{")) {
    scan();
    auto structDecLst = structDeclarationList();
    expect(PunctuatorType::RIGHTCURLYBRACE);
    scan();
    return make_shared<StructType>("",structDecLst);
  } else {
    expectedAnyOf();
  }
}


std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> Parser::structDeclarationList() {
  std::vector<decltype(structDeclaration())> declarations{}; 
  do {
    declarations.push_back(structDeclaration());
  } while (!testp("}")); 
  return declarations;
}

std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>> Parser::structDeclaration() {
  if(testTypeSpecifier()) {
    auto type = specifierQualifierList();
    if (testp(";")) {
      decltype(structDeclaration().second) empty {};
      scan();
      return std::make_pair(type, empty);
    } else {
     auto sdecll = structDeclaratorList();
     expect(PunctuatorType::SEMICOLON);
     scan();
     return make_pair(type, sdecll);
    }
  } else {
    expectedAnyOf(";");
  }
}

/*
specifier-qualifier-list ->   type-specifier
*/
TypeNode Parser::specifierQualifierList() {
  return typeSpecifier();
}

std::vector<std::pair<SubDeclarator,SubExpression>> Parser::structDeclaratorList() {
  decltype(structDeclaratorList()) structDeclarators {};
  structDeclarators.push_back(structDeclarator());

  while(testp(",")) {
    scan();
    structDeclarators.push_back(structDeclarator());
  }
  return structDeclarators;
}

std::pair<SubDeclarator,SubExpression> Parser::structDeclarator() {
  if (testp(":")) {
    /* TODO: when can this ever happen?!?
     * A structDeclarator without a declarator?
     */
    scan();
    // SubDeclarator will be an empty shared_ptr
    return std::make_pair(SubDeclarator(), constantExpression());
  } else {
    auto decl = declarator();
    if (testp(":")) {
      scan();
      return std::make_pair(decl, constantExpression());
    } else {
    // SubExpression will be an empty shared_ptr
      return std::make_pair(decl, SubExpression());
    }
  }
}

/*
parameter-list  ->   parameter-declaration
                   | parameter-list "," parameter-declaration
*/
std::vector<ParameterNode> Parser::parameterList() {
  decltype(parameterList()) parameters {};
  auto parameter = parameterDeclaration();
  parameters.push_back(parameter);
  while(testp(PunctuatorType::COMMA)) {
    scan();
    parameters.push_back(parameterDeclaration());
  }
  return parameters;
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
auto Parser::abstractDeclarator() -> decltype(Parser::declarator())  {
  return declarator(/*abstract =*/true);
}

/*
   direct-abstract-declarator ->  "(" abstract-declarator ")" direct-abstract-declarator_help

   direct-abstract-declarator_help -> "(" parameter-type-list ")" direct-abstract-declarator_help
   |   "(" ")" direct-abstract-declarator_help
   | EPSILON
   */
auto Parser::directAbstractDeclarator() -> decltype(Parser::directDeclarator()) {
  return directDeclarator(/*abstract =*/true);
}

auto Parser::directAbstractDeclaratorHelp() -> decltype(Parser::directAbstractDeclaratorHelp()) {
  return directDeclaratorHelp(/*abstract =*/true);
}

/*
 *
 parameter-declaration ->   declaration-specifiers declarator
 | declarations-specifiers abstract-declarator
 | declarations-specifiers
*/
ParameterNode Parser::parameterDeclaration() {
  auto type = typeSpecifier();
  if (testp(PunctuatorType::COMMA) || testp(PunctuatorType::RIGHTPARENTHESIS)) {
    return make_shared<Parameter>(type);
  } else {
    auto decl = declarator();
    // TODO distinguish abstract-declarator
    return make_shared<Parameter>(type, decl);
  }
}
/*
identifier-list ->  identifier
                  | identifier "," identifier-list
*/
SubIdentifierList Parser::identifierList() {
  std::vector<std::string> myList;

  expect(TokenType::IDENTIFIER);
  myList.push_back(m_nextsym->value());
  scan();

  while (testp(",")) {
    scan();

    expect(TokenType::IDENTIFIER);
    myList.push_back(m_nextsym->value());
    scan();
  }

  return make_shared<IdentifierList>(myList);
}

std::pair<TypeNode, SubDeclarator>  Parser::typeName() {
  auto type = specifierQualifierList();

  if (!testp(")")) {
    return make_pair(type, abstractDeclarator());
  } else {
    return make_pair(type, SubDeclarator());
  }
}

/*
declarator ->   pointer direct-declarator
              | direct-declarator
*/
SubDeclarator Parser::declarator(bool abstract) {
  int counter = 0;

  if(test(TokenType::PUNCTUATOR,"*")) {
    while(testp(PunctuatorType::STAR)) {
      counter++;
      scan();
    }
  }

  Pointer pointer(counter); // TODO: Is this still needed?
  SubDirectDeclarator dec = directDeclarator(abstract);

  return make_shared<Declarator>(counter, dec);
}

/*
direct-declarator -> identifier direct-declarator_help
                   | "(" declarator ")" direct-declarator_help

*/
SubDirectDeclarator Parser::directDeclarator(bool abstract) {
  if (!abstract && testType(TokenType::IDENTIFIER)) {
    std::string identifier = m_nextsym->value();
    scan();

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
      SubDirectDeclaratorHelp help =  directDeclaratorHelp();
      return make_shared<IdentifierDirectDeclarator>(identifier, help);
    } else {
      return make_shared<IdentifierDirectDeclarator>(identifier);
    }

  } else if (testp(PunctuatorType::LEFTPARENTHESIS)) {
    scan();


    SubDeclarator dec = declarator();

    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();
    } else {
      expectedAnyOf(std::string("direct-declarator : expected ')'"));
    }

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
      SubDirectDeclaratorHelp help = directDeclaratorHelp(abstract);
      return make_shared<DeclaratorDirectDeclarator>(dec,help);
    } else {
      return make_shared<DeclaratorDirectDeclarator>(dec);
    }
  } else {
    expectedAnyOf(std::string("error in direct Declarator"));
  }
}

/*
direct-declarator_help -> "(" parameter-list ")" direct-declarator_help
             | "(" identifier-list ")" direct-declarator_help
                          |  "(" ")" direct-declarator_help
                                       | EPSILON
*/
SubDirectDeclaratorHelp Parser::directDeclaratorHelp(bool abstract) {
  if (testp(PunctuatorType::LEFTPARENTHESIS)) { // TODO: use expect instead of testp
    scan();

    decltype(directDeclaratorHelp()) declHelp;
    // 1. option
    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        declHelp = directDeclaratorHelp(abstract);
        return make_shared<DirectDeclaratorHelp>(declHelp);
      } else {
        return make_shared<DirectDeclaratorHelp>();
      }

    } else if (testTypeSpecifier()) { // parameter-list
      auto params = parameterList();
      expect(PunctuatorType::RIGHTPARENTHESIS);
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        declHelp = directDeclaratorHelp(abstract);
        return make_shared<DirectDeclaratorHelp>(params, declHelp);
      }
      return make_shared<DirectDeclaratorHelp>(params);
    } else if(!abstract && testType(TokenType::IDENTIFIER)) {
      auto ids = identifierList();
      expect(PunctuatorType::RIGHTPARENTHESIS);
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        declHelp = directDeclaratorHelp();
        return make_shared<DirectDeclaratorHelp>(ids, declHelp);
      }
      return make_shared<DirectDeclaratorHelp>(ids);
    } else {
      expectedAnyOf();
    }

  } else {
    expectedAnyOf(std::string("direct-declatror_help : '(' expected"));
  }

}

/**
compound-statement -> "{" block-item-list "}"
                     |  "{" "}"
*/
SubCompoundStatement Parser::compoundStatement() {

  expect(PunctuatorType::LEFTCURLYBRACE);
  scan();

  decltype(blockItemList()) subStatements {}; 
  if (testp(PunctuatorType::RIGHTCURLYBRACE)) {
    scan();
    return make_shared<CompoundStatement>(subStatements);
  } else {
    subStatements = blockItemList();
    expect(PunctuatorType::RIGHTCURLYBRACE);
    scan();
    return make_shared<CompoundStatement>(subStatements);
  }
}

/*
block-item-list ->  block-item
                  | block-item block-item-list
*/
std::vector<BlockItem> Parser::blockItemList() {
  auto items = std::vector<BlockItem>();
  while(!testp(PunctuatorType::RIGHTCURLYBRACE)) {
    items.push_back(blockItem());
  }
  return items;
}

/*
block-item ->   declaration
              | statement
*/
/* TODO: BlockItem is still an AstNode! */
BlockItem Parser::blockItem() {
  if (testTypeSpecifier()) {
    return declaration();
  } else {
    return statement();
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
SubStatement Parser::statement() {

  if(testk(   KeywordType::GOTO) 
           || testk(KeywordType::CONTINUE) 
           || testk(KeywordType::BREAK) 
           || testk(KeywordType::RETURN)) {
    return jumpStatement();
  } else if(testp(PunctuatorType::LEFTCURLYBRACE)) {
    return compoundStatement();
  } else if(testk(KeywordType::IF)) {
    return selectionStatement();
  } else if(testk(KeywordType::WHILE) || testk(KeywordType::DO)) {
    return iterationStatement();
  } else if(testType(TokenType::IDENTIFIER)) {
    if (testLookAheadP(":")) {
      return labeledStatement();
    } else {
      return expressionStatement();
    }
  } else {
    return expressionStatement();
  } 
}

/*
 expression-statement -> ";" | expression ";"
 */
SubExpressionStatement Parser::expressionStatement() {
  if (testp(PunctuatorType::SEMICOLON)) {
    scan();
    return make_shared<ExpressionStatement>();
  } else {
    SubExpression ex = expression();

    if (testp(PunctuatorType::SEMICOLON)) {
      scan();
    } else {
      expectedAnyOf();
    }

    return make_shared<ExpressionStatement>(ex);
  }
}

/*
labeled-statement -> identifier : statement
*/
SubLabeledStatement Parser::labeledStatement() {
  if(testType(TokenType::IDENTIFIER)) {
    std::string label = m_nextsym->value();
    scan();
    expect(PunctuatorType::COLON);
    scan();

    SubStatement st = statement();

    return make_shared<LabeledStatement>(label,st);
  } else {
    expectedAnyOf(std::string("labeled-statement : identifier expected"));
  }
}

/*
iteration-statement ->  "while" "(" expression ")" statement
                      | "do" statement "while" "(" expression ")" ";"

*/
SubIterationStatement Parser::iterationStatement() {
  if(testk(KeywordType::WHILE)) {
    scan();
    expect(PunctuatorType::LEFTPARENTHESIS);
    scan();
    SubExpression ex = expression();
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    SubStatement st = statement();
    return make_shared<IterationStatement>(ex, st, IterationEnum::WHILE);
  } else if (testk(KeywordType::DO)) {
    scan();
    SubStatement st = statement();
    expect("while");
    scan();
    expect("(");
    scan();
    SubExpression ex = expression();
    expect(")");
    scan();
    expect(";");
    scan();

    return make_shared<IterationStatement>(ex, st, IterationEnum::DOWHILE);

  } else {
    expectedAnyOf(std::string("iteration-statement : no match found"));
  }
  
}

/*
selection-statement ->   "if" "(" expression ")" statement
   | "if" "(" expression ")" statement "else" statement
*/

SubSelectionStatement Parser::selectionStatement() {
  if (testk(KeywordType::IF)) {
    scan();

    expect("(");
    scan();
    SubExpression ex = expression();
    expect(")");
    scan();
    SubStatement st1 = statement();

    if(testk(KeywordType::ELSE)) {
      scan();
      SubStatement st2 = statement();
      return make_shared<SelectionStatement>(ex, st1, st2);
    } else {
      return make_shared<SelectionStatement>(ex, st1);
    }
  } else {
    expectedAnyOf(std::string("selectionStatement: no match"));
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
      expectedAnyOf(std::string("jump-statement: identifier expected"));
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
    expectedAnyOf(std::string("jump-statement : unexpected token"));
  }
}

SubExpression Parser::constantExpression() {
  // TODO: use constant expression
  return expression();
}

