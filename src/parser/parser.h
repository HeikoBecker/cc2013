#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <vector>
#include <unordered_set>
#include <sstream>
#include <stdexcept>
#include <memory>
#include "ast.h"
#include "semantic.h"
#include "../lexer/token.h"

namespace Lexing {
  class Lexer;
}

using namespace std;
using namespace Lexing;

namespace Parsing {

  class ParsingException: public std::runtime_error 
  {
    public:
      ParsingException(std::string message, Pos pos) 
        : std::runtime_error(message), pos(pos) {};
      Pos pos;
  };

  enum class ThreeValueBool {
    DONTCARE, 
    ABSTRACT,
    NOTABSTRACT
  };

  /* class for the Parsing
   * the parser takes a list of tokens
   * and checks whether it is valid
   * it should also build the tree
   */
  class Parser
  {
    public:
      Parser(FILE* f, char const *name);
      AstRoot parse();

    private:
      unique_ptr<Lexer> m_lexer;
      shared_ptr<Token> m_nextsym;
      shared_ptr<Token> m_lookahead;
      shared_ptr<SemanticTree> semanticTree;

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
      inline void expect(std::string s);
      inline void expect(PunctuatorType puncutator);
      inline void expect(KeywordType keyword);
      inline void expect(TokenType tokenType);
      [[noreturn]] inline void reportError(std::string msg);
      [[noreturn]] inline void expectedAnyOf(std::string msg);


      // debug function 
      void debugOutput();

      // parse functions
      std::vector<BlockItem> blockItemList();
      BlockItem blockItem();


      // statements
      SubStatement statement();
      SubJumpStatement jumpStatement();
      SubSelectionStatement selectionStatement();
      SubIterationStatement iterationStatement();
      SubLabeledStatement labeledStatement();

      SubExpression expression(int minPrecedence);
      SubExpression computeAtom();
      SubExpression postfixExpression(SubExpression child);
      SubExpression sizeOfType();
      SubExpression constantExpression();
      DeclarationNode declaration();
      SubDeclarator declarator(ThreeValueBool abstract = ThreeValueBool::NOTABSTRACT);
      SubDeclarator abstractDeclarator();
      SubDirectDeclarator directDeclarator(ThreeValueBool abstract = ThreeValueBool::NOTABSTRACT);
      SubDirectDeclarator directAbstractDeclarator();
      SubDirectDeclaratorHelp directDeclaratorHelp(ThreeValueBool abstract = ThreeValueBool::NOTABSTRACT);
      SubDirectDeclaratorHelp directAbstractDeclaratorHelp();
      TypeNode typeSpecifier();
      StructNode structOrUnionSpecifier();
      std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>> structDeclaration();
      std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> structDeclarationList();
      TypeNode specifierQualifierList();
      std::vector<std::pair<SubDeclarator,SubExpression>> structDeclaratorList();
      std::pair<SubDeclarator,SubExpression> structDeclarator();
      std::vector<ParameterNode> parameterList();
      ParameterNode parameterDeclaration();
      SubIdentifierList identifierList();
      std::pair<TypeNode, SubDeclarator>  typeName();
      ExternalDeclarationNode externalDeclaration();
      TUNode translationUnit();

      SubCompoundStatement compoundStatement();
      SubExpressionStatement expressionStatement();
  };
}

#endif
