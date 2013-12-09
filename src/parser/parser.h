#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <vector>
#include <unordered_set>
#include <sstream>
#include <stdexcept>
#include <memory>
#include "ast.h"
#include "../lexer/token.h"

namespace Lexing {
  class Lexer;
}

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
      Parser(FILE* f, char const *name);
      bool parse();

    private:
      unique_ptr<Lexer> m_lexer;
      shared_ptr<Token> m_nextsym;
      shared_ptr<Token> m_lookahead;

      // function concerning reading
      shared_ptr<Token> getNextSymbol();
      TokenType getNextType();
      string getNextValue();
      shared_ptr<Token> scan();
      bool testType(TokenType token);
      bool testValue(string value);
      bool test(TokenType token, string value);
      bool testp(string value); // test punctuator
      bool testp(PunctuatorType puncutator); // test punctuator
      bool testk(string value); // test keyword
      bool testk(KeywordType keyword); // test keyword
      bool testTypeSpecifier();
      bool testLookAheadP(string val);
      void readSemicolon(string funcName);
      inline void expect(std::string s);
      inline void expect(PunctuatorType puncutator);
      inline void expect(KeywordType keyword);
      inline void expect(TokenType tokenType);
      inline void reportError();


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

      SubExpression expression(int minPrecedence);
      SubExpression computeAtom();
      SubExpression postfixExpression(SubExpression child);
      SubExpression sizeOfType();
      void primaryExpression();
      void postfixExpression();
      void argumentExpressionList();
      void unaryExpression();
      void constantExpression();
      void unaryOperator(); // TODO: probably not needed
      void additiveExpression();
      void multiplicativeExpression();
      void castExpression();
      void declaration();
      void declarationSpecifiers();
      void directDeclarator();
      void directDeclaratorHelp();
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
      void externalDeclaration();
      void translationUnit();
      void compoundStatement();
      void expressionStatement();
      void staticAssert();
      void abstractDeclarator();
      void directAbstractDeclarator();
      void directAbstractDeclaratorHelp();
  };
}

#endif
