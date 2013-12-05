#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <vector>
#include <unordered_set>
#include <sstream>
#include <stdexcept>
#include "ast.h"
#include "../lexer/token.h"

using namespace std;
using namespace Lexing;

namespace Parsing {

  /* class for the Parsing
   * the parser takes a list of tokens
   * and checks whether it is valid
   * it should also build the tree
   */
  class Parser
  {
    public:
      Parser(vector<Token> token);
      bool parse();

    private:
      vector<Token> m_tokenList;
      Token m_nextsym;
      int posTokenList;


      // function concerning reading
      Token getNextSymbol();
      TokenType getNextType();
      string getNextValue();
      Token scan();
      bool testType(TokenType token);
      bool testValue(string value);
      bool test(TokenType token, string value);
      bool testp(string value); // test punctuator
      bool testk(string value); // test keyword
      bool testTypeSpecifier();
      void readP(string value); // read punctuator 
      void readK(string value); // read keyword

      void readSemicolon(string funcName);

      // debug function 
      void debugOutput();

      // parse functions
      void blockItemList();
      void blockItem();


      // statements
      void statement();
      void jumpStatement();
      void selectionStatement();
      void iterationStatement();
      void labeledStatement();

      AstChild expression(int minPrecedence);
      AstChild computeAtom();
      void primaryExpression();
      void postfixExpression();
      void argumentExpressionList();
      void unaryExpression();
      void unaryOperator(); // TODO: probably not needed
      void additiveExpression();
      void multiplicativeExpression();
      void castExpression();
      void declaration();
      void declarationSpecifiers();
      void directDeclarator();
      void directDeclaratorHelp();
      void initDeclaratorList();
      void initDeclarator();
      void typeSpecifier();
      void structOrUnion();
      void structOrUnionSpecifier();
      void structDeclarationList();
      void structDeclaration();
      void specifierQualifierList();
      void structDeclaratorList();
      void structDeclarator();
      void enumSpecifier();
      void enumeratorList();
      void enumerator();
      void pointer();
      void parameterTypeList();
      void parameterList();
      void parameterDeclaration();
      void identifierList();
      void typeName();
      void declarator();
      void directOrAbstractDeclarator(bool isDirect);
      void functionDefinition();
      void externalDeclaration();
      void translationUnit();
      void compoundStatement();
  };
}

#endif
