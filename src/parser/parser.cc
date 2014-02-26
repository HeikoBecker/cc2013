#include <sstream>
#include <iostream>
#include <exception>
#include <cstdio>
#include <cctype>
#include <utility> // for std::move
#include "parser.h"
#include "../utils/diagnostic.h"
#include "../utils/debug.h"
#include "../lexer/lexer.h"
#include "../lexer/punctuatortype.h"
#include "../lexer/keywordtokentype.h"

#define OBTAIN_POS() auto pos = m_nextsym->pos();

using namespace std;
using namespace Lexing;
using namespace Parsing;


// init parser
Parser::Parser(FILE* f, char const *name)
  :  m_lexer(unique_ptr<Lexer>(new Lexer(f,name))) ,
     m_nextsym(m_lexer->getNextToken()), m_lookahead(m_lexer->getNextToken())
{
  semanticTree = SemanticForest::filename2SemanticTree(name);
}

[[noreturn]] inline void Parser::reportError( Pos pos, std::string msg = "Parsing error") {
  //errorf(m_nextsym->pos(), msg.c_str()); FIXME: decide where to use exceptions
  // TODO: the full featured parser should continue
  throw ParsingException(msg, pos);
}

[[noreturn]] inline void Parser::reportError(std::string msg = "Parsing error") {
  //errorf(m_nextsym->pos(), msg.c_str()); FIXME: decide where to use exceptions
  // TODO: the full featured parser should continue
  OBTAIN_POS();
  throw ParsingException(msg, pos);
}

[[noreturn]] void Parser::expectedAnyOf(std::string msg = "Parsing error") {
  msg += std::string("\nLast read symbol was ");
  msg += m_nextsym->value();
  reportError(msg);
}

void Parser::expect(PunctuatorType puncutator) {
  if (!testp(puncutator)) {
    expected(Lexing::PunctuatorType2String(puncutator));
  }
}

void Parser::expect(KeywordType keyword) {
  if (!testk(keyword)) {
    expected(KeywordType2String(keyword));
  }
}

void Parser::expect(TokenType tokenType) {
  if (getNextType() != tokenType) {
    switch(tokenType) {
      case TokenType::IDENTIFIER:
        expected("an identifier");
        break;
      case TokenType::CONSTANT:
        expected("a constant");
        break;
      case TokenType::STRINGLITERAL:
        expected("a stringliteral");
        break;
      default:
        expected("that this is handled by the other expect functions!");
        break;
    }
  }
}

void Parser::expected(std::string expected) {
    auto msg = std::string("Expected ");
    msg += expected;
    msg += std::string(" but got ");
    msg += m_nextsym->value();
    if (m_nextsym->type() == TokenType::END) {
      msg += "EOF!";
    }
    reportError(msg);
}

bool Parser::testTypeSpecifier() {
  return testType(TokenType::KEYWORD) && (
    testValue("void") || testValue("int") ||
    testValue("char") || testValue("struct")
  );
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


bool Parser::testLookAheadType() {
  return (m_lookahead->value() == "int"     || 
          m_lookahead->value() == "char"    || 
          m_lookahead->value() == "void"    || 
          m_lookahead->value() == "struct") && 
         m_lookahead->type() == TokenType::KEYWORD;
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
  m_nextsym = std::move(m_lookahead);
  m_lookahead = m_lexer->getNextToken();
  return m_nextsym;
}

AstRoot Parser::parse() {
  auto tu = translationUnit();
  expect(TokenType::END);

  return tu;
}

TUNode Parser::translationUnit() {
  OBTAIN_POS();
  std::vector<ExternalDeclarationNode> externalDeclarations {};
  while (!testType(TokenType::END)) {
     externalDeclarations.push_back(externalDeclaration());
  }
  return make_shared<TranslationUnit>(externalDeclarations, pos);
}

ExternalDeclarationNode Parser::externalDeclaration() {

  /* TODO: avoid code duplication with declaration
   * the start of declaration is !exactly! the same as the one for external
   * declaration
   */
  // functionDefinition or declaration ?
  OBTAIN_POS();

  auto type = typeSpecifier();
  if (testp(";")) {
    // it was a declaration
    scan();

    // TODO : make lookuptable for struct
    // or is this not allowed in our grammar
    return make_shared<ExternalDeclaration>(type, pos, semanticTree);
  }

  auto decl = declarator();

  if (testp(";")) {
    scan();
    // it was a declaration()
    return make_shared<ExternalDeclaration>(type, decl, pos, semanticTree);
  }

  // it is a functionDefition!
  if (!decl->canBeFunctionDefinition()) {
    reportError(decl->pos(), "FunctionDefinition has to be either (void) or all parameters have to be declared");
  }

  expect(PunctuatorType::LEFTCURLYBRACE);

  auto parameter = decl->getNextParameter();


  auto returnType = semanticTree->addDeclaration(type, decl, pos, false);
  semanticTree->setCurrentFunction(returnType);

  auto compStat = compoundStatement(parameter);
  semanticTree->unsetCurrentFunction();

  return make_shared<FunctionDefinition>(type, decl, compStat, 
                                          pos, semanticTree);
}

// canBeFunction is true at the beginning
TypeNode Parser::typeSpecifier() {
  OBTAIN_POS();
  if (testk("struct")) {
    return structOrUnionSpecifier();
  } else {
    auto type = std::make_shared<BasicType>(m_nextsym->value(), pos);
    scan();
    if (testp(PunctuatorType::SEMICOLON) && !type->containsDeclaration()) {
      throw ParsingException("Declaration doesn't declare anything!", pos);
    }
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
    debug(GENERAL) << t.value();
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
      || t.value() == "?"
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
  OBTAIN_POS();
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
        child = make_shared<FunctionCall>(child, arguments, pos);
      } else if (testp(PunctuatorType::LEFTSQBRACKET)) { //array access(?). TODO: Are expressions supported in []?
        scan();
        auto index = expression(0);
        expect(PunctuatorType::RIGHTSQBRACKET);
        scan();
        child = make_shared<BinaryExpression>(child,index, PunctuatorType::ARRAY_ACCESS, pos);
      } else if (testp(PunctuatorType::ARROW) || testp(PunctuatorType::MEMBER_ACCESS)) { // member access
        PunctuatorType p = (testp(PunctuatorType::MEMBER_ACCESS)) ? PunctuatorType::MEMBER_ACCESS
                                        : PunctuatorType::ARROW;
        scan();
        expect(TokenType::IDENTIFIER);
        auto var = make_shared<VariableUsage>(m_nextsym->value(), pos, semanticTree);
        child = make_shared<BinaryExpression>(child, var, p, pos);
        scan();
      } else {
        cont = !cont;
      }
    }
    return child;
}

SubExpression Parser::computeAtom() {
  OBTAIN_POS();
  if (testp(PunctuatorType::LEFTPARENTHESIS)) { 
    // parse expression in parentheses
    scan();
    auto child = expression(0);
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    // handle postfix-expression
    child = postfixExpression(child);
    return child;
  } else if ( m_nextsym->type() == TokenType::IDENTIFIER ) {
    // 'normal ' atom, variable 
    // maybe followed by one of ., ->, [], ()
    auto var = std::make_shared<VariableUsage>(m_nextsym->value(), pos, semanticTree);
    scan();
    auto child = SubExpression(var);
    // handle postfix-expression
    child = postfixExpression(child);
    return child;
  } else if ( m_nextsym->type() == TokenType::CONSTANT) {
    // 'normal ' atom, constant
    auto ct = std::static_pointer_cast<ConstantToken>(m_nextsym)->type;
    auto var = std::make_shared<Constant>(m_nextsym->value(), pos, ct);
    //auto var = std::make_shared<Literal>(m_nextsym->value(), pos);
    scan();
    auto child = SubExpression(var);
    child = postfixExpression(child);
    return child;
  } else if (m_nextsym->type() == TokenType::STRINGLITERAL) {
    // 'normal ' atom, literal
    auto var = std::make_shared<Literal>(m_nextsym->value(), pos);
    scan();
    auto child = SubExpression(var);
    child = postfixExpression(child);
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
    return make_shared<UnaryExpression>(op, operand, pos);
  } else {
    // something went wrong
    // TODO: LATER: return error expression object
    expectedAnyOf();
    shared_ptr<Expression> errorneous;
    return errorneous;
  }
}

SubExpression Parser::sizeOfType() {
  OBTAIN_POS();
  // TODO: actually construct AST class
  scan(); // read starting parenthesis
  // read type
  auto type = typeName();
  expect(PunctuatorType::RIGHTPARENTHESIS);
  scan();  // read closing parenthesis
  return make_shared<SizeOfExpression>(type, pos);
}

SubExpression Parser::expression(int minPrecedence = 0) {
  OBTAIN_POS();
  auto expr = computeAtom();
  SubExpression ternaryHelper;
  while (isBinaryOperator(*m_nextsym) && getPrec(*m_nextsym) >= minPrecedence) {
    auto operator_position = m_nextsym->pos();
    auto punctype = static_pointer_cast<PunctuatorToken>(m_nextsym)->punctype();
    auto isTernary = punctype == PunctuatorType::QMARK;
    int precNext;
    if (isTernary) {
      //auto prec_ternary = getPrec(*m_nextsym);
      expect(PunctuatorType::QMARK);
      scan(); // read the ?
      ternaryHelper = expression(1/*prec_ternary*/); //FIXME: with prec_ternary (== 2) it doesn't work; but this is a hack...
      expect(PunctuatorType::COLON);
      precNext = 2;
    } else {
      precNext = (isRightAssociative(*m_nextsym))
        ? getPrec(*m_nextsym)
        : getPrec(*m_nextsym) + 1;
    }
    scan(); // this will either read the binary operator or ":" if we're parsing the ternary operator
    auto rhs = expression(precNext);
    if (isTernary) {
      expr = make_shared<TernaryExpression>(expr, ternaryHelper, rhs, pos);
    } else {
      expr = make_shared<BinaryExpression>(expr, rhs, punctype, pos, &operator_position);
    }
  }
  return expr;
}

DeclarationNode Parser::declaration() {
  OBTAIN_POS();

  auto type = typeSpecifier();

  if (testp(";")) {
    scan();
    return std::make_shared<Declaration>(type, pos);
  } else {
    auto decl = declarator();
    expect(PunctuatorType::SEMICOLON);
    scan();
    return std::make_shared<Declaration>(type, decl, pos, semanticTree);
  }
}

string structInPlace ="";

/*
struct-or-union-specifier -> "struct" identifier "{" struct-declarations-list "}"
                            | "struct" "{" struct-declarations-list "}"
                                                        | "struct"  identifier
*/
StructNode Parser::structOrUnionSpecifier() {
  OBTAIN_POS();
  expect(KeywordType::STRUCT);
  scan();

  if (testType(TokenType::IDENTIFIER)) {
    auto name = m_nextsym->value();


    scan();
    if (testp(PunctuatorType::LEFTCURLYBRACE)) {
      semanticTree->addChild(pos, "@"+name);
      expect(PunctuatorType::LEFTCURLYBRACE);
      scan();
      auto structDecLst = structDeclarationList();
      expect(PunctuatorType::RIGHTCURLYBRACE);
      scan();
      auto ret =  make_shared<StructType>(name, structDecLst, pos);
      semanticTree->goUp();
      return ret;
    } else if(!testp(PunctuatorType::SEMICOLON)) {
      // check for struct S something;
      string myname = "@"+ name;
      if (!semanticTree->hasStructDeclaration(myname)) {
        semanticTree->addChild(pos, myname, true);
        semanticTree->goUp();
      }
    }
    return make_shared<StructType>(name, pos);
  } else  if (testp(PunctuatorType::LEFTCURLYBRACE)) {
      structInPlace = structInPlace+"u";
      
      string name = "@@" + structInPlace;
      string type = "@" + structInPlace;


      semanticTree->addChild(pos, name);
      expect(PunctuatorType::LEFTCURLYBRACE);
      scan();
      auto structDecLst = structDeclarationList();
      expect(PunctuatorType::RIGHTCURLYBRACE);
      scan();
      auto ret =  make_shared<StructType>(type, structDecLst, pos);
      semanticTree->goUp();
      return ret;
    }
  else {
    expectedAnyOf();
  }
}


std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> Parser::structDeclarationList() {
  std::vector<decltype(structDeclaration())> declarations{}; 
  do {
    declarations.push_back(structDeclaration());
  } while (!testp(PunctuatorType::RIGHTCURLYBRACE)); 
  return declarations;
}

std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>> Parser::structDeclaration() {
  if(testTypeSpecifier()) {
    // TODO: we should be able to reuse parts of declaration here
    auto type = specifierQualifierList();
    if (testp(";")) {
      decltype(structDeclaration().second) empty {};
      scan();
      return std::make_pair(type, empty);
    } else {
     auto sdecll = structDeclaratorList(type);
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

std::vector<std::pair<SubDeclarator,SubExpression>> 
  Parser::structDeclaratorList(TypeNode type) {
  decltype(structDeclaratorList(type)) structDeclarators {};

  structDeclarators.push_back(structDeclarator(type));

  while(testp(",")) {
    scan();
    structDeclarators.push_back(structDeclarator(type));
  }
  return structDeclarators;
}

std::pair<SubDeclarator,SubExpression> Parser::structDeclarator(TypeNode type) {
  if (testp(":")) {
    /* TODO: when can this ever happen?!?
     * A structDeclarator without a declarator?
     */
    scan();
    // SubDeclarator will be an empty shared_ptr
    return std::make_pair(SubDeclarator(), constantExpression());
  } else {

    OBTAIN_POS();
    auto decl = declarator();
    
    semanticTree->addDeclaration(type, decl, pos);

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
  return declarator(ThreeValueBool::ABSTRACT);
}

/*
   direct-abstract-declarator ->  "(" abstract-declarator ")" direct-abstract-declarator_help

   direct-abstract-declarator_help -> "(" parameter-type-list ")" direct-abstract-declarator_help
   |   "(" ")" direct-abstract-declarator_help
   | EPSILON
   */
auto Parser::directAbstractDeclarator() -> decltype(Parser::directDeclarator()) {
  return directDeclarator(ThreeValueBool::ABSTRACT);
}

auto Parser::directAbstractDeclaratorHelp(std::vector<SubDirectDeclaratorHelp> & hs) -> decltype(Parser::directAbstractDeclaratorHelp(hs)) {
  directDeclaratorHelp(hs, ThreeValueBool::ABSTRACT);
}

/*
 *
 parameter-declaration ->   declaration-specifiers declarator
 | declarations-specifiers abstract-declarator
 | declarations-specifiers
*/
ParameterNode Parser::parameterDeclaration() {
  OBTAIN_POS();
  auto type = typeSpecifier();


  if (testp(PunctuatorType::COMMA) || testp(PunctuatorType::RIGHTPARENTHESIS)) {
    return make_shared<Parameter>(type, pos);
  } else {
    auto decl = declarator(ThreeValueBool::DONTCARE);
    return make_shared<Parameter>(type, decl, pos);
  }
}
/*
identifier-list ->  identifier
                  | identifier "," identifier-list
*/
SubIdentifierList Parser::identifierList() {
  OBTAIN_POS();
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

  return make_shared<IdentifierList>(myList, pos);
}

std::pair<TypeNode, SubDeclarator>  Parser::typeName() {
  auto type = specifierQualifierList();

  if (!testp(PunctuatorType::RIGHTPARENTHESIS)) {
    return make_pair(type, abstractDeclarator());
  } else {
    return make_pair(type, SubDeclarator());
  }
}

/*
declarator ->   pointer direct-declarator
              | direct-declarator
*/
SubDeclarator Parser::declarator(ThreeValueBool abstract) {
  OBTAIN_POS();
  int counter = 0;

  if(test(TokenType::PUNCTUATOR,"*")) {
    while(testp(PunctuatorType::STAR)) {
      counter++;
      scan();
    }
  }

  Pointer pointer(counter, pos); // TODO: Is this still needed?
  // TODO: is the if below correct?
  if (testp(PunctuatorType::RIGHTPARENTHESIS)
    || testp(PunctuatorType::COMMA) // in parameterlist
    ) {

 return make_shared<Declarator>(counter,
        decltype(directDeclarator(abstract))(),
        pos);


  } else {
    SubDirectDeclarator dec = directDeclarator(abstract);
    return make_shared<Declarator>(counter, dec, pos);
 }
}

/*
direct-declarator -> identifier direct-declarator_help
                   | "(" declarator ")" direct-declarator_help

*/
SubDirectDeclarator Parser::directDeclarator(ThreeValueBool abstract) {
  OBTAIN_POS();
  if (   (   abstract == ThreeValueBool::NOTABSTRACT
          || abstract == ThreeValueBool::DONTCARE)
      && testType(TokenType::IDENTIFIER)) {
    std::string identifier = m_nextsym->value();
    scan();

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
      std::vector<SubDirectDeclaratorHelp> help;
      directDeclaratorHelp(help, ThreeValueBool::NOTABSTRACT);
      return make_shared<IdentifierDirectDeclarator>(identifier, help, pos);
    } else {
      return make_shared<IdentifierDirectDeclarator>(identifier, pos);
    }

  } else if (testp(PunctuatorType::LEFTPARENTHESIS)) {
    
    // bugfix
    if( 
      (abstract == ThreeValueBool::ABSTRACT ||
      abstract == ThreeValueBool::DONTCARE) &&
      testLookAheadType()) {
      SubDeclarator dec2;
      std::vector<SubDirectDeclaratorHelp> help;
      directDeclaratorHelp(help, abstract);
      return make_shared<DeclaratorDirectDeclarator>(dec2,help, pos);
    }

    scan();
    
    SubDeclarator dec = declarator(abstract);

    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();
    } else {
      expectedAnyOf(std::string("direct-declarator : expected ')'"));
    }

    if(testp(PunctuatorType::LEFTPARENTHESIS)) {
      std::vector<SubDirectDeclaratorHelp> help;
      directDeclaratorHelp(help, abstract);
      return make_shared<DeclaratorDirectDeclarator>(dec,help, pos);
    } else {
      return make_shared<DeclaratorDirectDeclarator>(dec, pos);
    }
  } else {
    if (abstract == ThreeValueBool::ABSTRACT) {
      expectedAnyOf(std::string("error in abstract direct Declarator"));
    } else if (abstract == ThreeValueBool::NOTABSTRACT) {
      expectedAnyOf(std::string("error in direct Declarator"));
    } else {
      expectedAnyOf(std::string("error in dontcare direct Declarator"));
    }
  }
}

/*
direct-declarator_help -> "(" parameter-list ")" direct-declarator_help
             | "(" identifier-list ")" direct-declarator_help
                          |  "(" ")" direct-declarator_help
                                       | EPSILON
*/
void Parser::directDeclaratorHelp(std::vector<SubDirectDeclaratorHelp> & hs ,ThreeValueBool abstract) {
  OBTAIN_POS();


  if (testp(PunctuatorType::LEFTPARENTHESIS)) { // TODO: use expect instead of testp
    scan();


    // 1. option
    if(testp(PunctuatorType::RIGHTPARENTHESIS)) {
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        directDeclaratorHelp(hs, abstract);
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(pos));
      } else {
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(pos));
      }

    } else if (testTypeSpecifier()) { // parameter-list
      
      auto params = parameterList();
      expect(PunctuatorType::RIGHTPARENTHESIS);
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        directDeclaratorHelp(hs, abstract);
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(params, pos));
      } else {
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(params, pos));
      }
    } else if((abstract == ThreeValueBool::NOTABSTRACT || abstract == ThreeValueBool::DONTCARE) && testType(TokenType::IDENTIFIER)) {
      auto ids = identifierList();
      expect(PunctuatorType::RIGHTPARENTHESIS);
      scan();

      if(testp(PunctuatorType::LEFTPARENTHESIS)) {
        directDeclaratorHelp(hs, ThreeValueBool::NOTABSTRACT);
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(ids, pos));
      } else {
        hs.emplace_back(make_shared<DirectDeclaratorHelp>(ids, pos));
      }
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
SubCompoundStatement Parser::compoundStatement(vector<ParameterNode> paramList ) {
  OBTAIN_POS();

  // add a new child to the semantic tree
  // and go to that child
  semanticTree->addChild(pos);

  // add variables for the function
  for (auto par : paramList) {
    semanticTree->addDeclaration(par->getType(), par->getDeclarator(), pos);
  }

  expect(PunctuatorType::LEFTCURLYBRACE);
  scan();

  decltype(blockItemList()) subStatements {}; 
  if (testp(PunctuatorType::RIGHTCURLYBRACE)) {
    scan();
    auto statement = make_shared<CompoundStatement>(subStatements, pos);
    semanticTree->goUp();
    return statement;
  } else {
    subStatements = blockItemList();
    expect(PunctuatorType::RIGHTCURLYBRACE);
    scan();
    auto statement = make_shared<CompoundStatement>(subStatements, pos);
    semanticTree->goUp();
    return statement;
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
  OBTAIN_POS();
  if (testp(PunctuatorType::SEMICOLON)) {
    scan();
    return make_shared<ExpressionStatement>(pos);
  } else {
    SubExpression ex = expression();

    if (testp(PunctuatorType::SEMICOLON)) {
      scan();
    } else {
      expectedAnyOf();
    }

    return make_shared<ExpressionStatement>(ex, pos);
  }
}

/*
labeled-statement -> identifier : statement
*/
SubLabeledStatement Parser::labeledStatement() {
  OBTAIN_POS();
  if(testType(TokenType::IDENTIFIER)) {
    std::string label = m_nextsym->value();
    scan();
    expect(PunctuatorType::COLON);
    scan();

    SubStatement st = statement();
    bool unique = semanticTree->addLabel(label);

    if(!unique) {
      reportError(pos, "The label " + label + " is already defined");
    }
    
    return make_shared<LabeledStatement>(label, st, pos);
  } else {
    expectedAnyOf(std::string("labeled-statement : identifier expected"));
  }
}

/*
iteration-statement ->  "while" "(" expression ")" statement
                      | "do" statement "while" "(" expression ")" ";"

*/
SubIterationStatement Parser::iterationStatement() {
  OBTAIN_POS();

  // we can have break and continue here
  semanticTree->increaseLoopDepth();

  if(testk(KeywordType::WHILE)) {
    scan();
    expect(PunctuatorType::LEFTPARENTHESIS);
    scan();
    SubExpression ex = expression();
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    SubStatement st = statement();
    semanticTree->decreaseLoopDepth();
    return make_shared<IterationStatement>(ex, st, IterationEnum::WHILE, pos);
  } else if (testk(KeywordType::DO)) {
    scan();
    SubStatement st = statement();
    expect(KeywordType::WHILE);
    scan();
    expect(PunctuatorType::LEFTPARENTHESIS);
    scan();
    SubExpression ex = expression();
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    expect(PunctuatorType::SEMICOLON);
    scan();
    semanticTree->decreaseLoopDepth();
    return make_shared<IterationStatement>(ex, st, IterationEnum::DOWHILE, pos);

  } else {
    expectedAnyOf(std::string("iteration-statement : no match found"));
  }
  
}

/*
selection-statement ->   "if" "(" expression ")" statement
   | "if" "(" expression ")" statement "else" statement
*/

SubSelectionStatement Parser::selectionStatement() {
  OBTAIN_POS();
  if (testk(KeywordType::IF)) {
    scan();

    expect(PunctuatorType::LEFTPARENTHESIS);
    scan();
    SubExpression ex = expression();
    expect(PunctuatorType::RIGHTPARENTHESIS);
    scan();
    SubStatement st1 = statement();

    if(testk(KeywordType::ELSE)) {
      scan();
      SubStatement st2 = statement();
      return make_shared<SelectionStatement>(ex, st1, st2, pos);
    } else {
      return make_shared<SelectionStatement>(ex, st1, pos);
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
  OBTAIN_POS();
  if (testk(KeywordType::GOTO)) {
    scan();
    if(testType(TokenType::IDENTIFIER)) {

      semanticTree->addGotoLabel(m_nextsym->value(), pos);

      SubJumpStatement gotoStatement = make_shared<GotoStatement>(m_nextsym->value(), pos);
      scan();

      expect(PunctuatorType::SEMICOLON);
      scan();

      return gotoStatement;
    } else {
      expectedAnyOf(std::string("jump-statement: identifier expected"));
    }
  } else if (testk(KeywordType::CONTINUE)) {
    if (!semanticTree->isInLoop()) {
      reportError("continue must be inside a loop");
    }

    scan();
    expect(PunctuatorType::SEMICOLON);
    scan();

    return make_shared<ContinueStatement>(pos);
  } else if (testk(KeywordType::BREAK)) {
    if (!semanticTree->isInLoop()) {
      reportError("break must be inside a loop");
    }
    scan();
    expect(PunctuatorType::SEMICOLON);
    scan();
    return make_shared<BreakStatement>(pos);
  } else if (testk(KeywordType::RETURN)){
    scan();

    if(testp(PunctuatorType::SEMICOLON)) {
      scan();

      return make_shared<ReturnStatement>(pos);
    } else {
      SubExpression sub = expression();
      expect(PunctuatorType::SEMICOLON);
      scan();

      return make_shared<ReturnStatement>(sub, pos);
    }
  } else {
    expectedAnyOf(std::string("jump-statement : unexpected token"));
  }
}

SubExpression Parser::constantExpression() {
  // TODO: use constant expression
  return expression();
}
