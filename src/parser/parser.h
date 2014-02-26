#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <vector>
#include <unordered_set>
#include <memory>
#include <string>
#include "ast.h"
#include "semantic.h"
#include "../lexer/token.h"

namespace Lexing {
  class Lexer;
}

namespace Parsing {

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
      std::unique_ptr<Lexing::Lexer> m_lexer;
      std::shared_ptr<Lexing::Token> m_nextsym;
      std::shared_ptr<Lexing::Token> m_lookahead;
      std::shared_ptr<SemanticTree> semanticTree;

      // function concerning reading
      std::shared_ptr<Lexing::Token> getNextSymbol();
      Lexing::TokenType getNextType();
      std::string getNextValue();
      std::shared_ptr<Lexing::Token> scan();
      bool testType(Lexing::TokenType token);
      bool testValue(std::string value);
      bool test(Lexing::TokenType token, std::string value);
      bool testp(std::string value); // test punctuator
      bool testp(PunctuatorType puncutator); // test punctuator
      bool testk(std::string value); // test keyword
      bool testk(KeywordType keyword); // test keyword
      bool testTypeSpecifier();
      bool testLookAheadP(std::string val);
      bool testLookAheadType();
      void expected(std::string expected);
      void expect(PunctuatorType puncutator);
      void expect(KeywordType keyword);
      void expect(Lexing::TokenType tokenType);
      [[noreturn]] inline void reportError(Pos pos, std::string msg);
      [[noreturn]] inline void reportError(std::string msg);
      [[noreturn]] inline void expectedAnyOf(std::string msg);


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
      void directDeclaratorHelp(std::vector<SubDirectDeclaratorHelp> & hs,
                                ThreeValueBool abstract = ThreeValueBool::NOTABSTRACT);
      void directAbstractDeclaratorHelp(std::vector<SubDirectDeclaratorHelp> & hs);
      TypeNode typeSpecifier();
      StructNode structOrUnionSpecifier();
      std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>> structDeclaration();
      std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> structDeclarationList();
      TypeNode specifierQualifierList();
      std::vector<std::pair<SubDeclarator,SubExpression>> structDeclaratorList(TypeNode type);
      std::pair<SubDeclarator,SubExpression> structDeclarator(TypeNode type);
      std::vector<ParameterNode> parameterList();
      ParameterNode parameterDeclaration();
      SubIdentifierList identifierList();
      std::pair<TypeNode, SubDeclarator>  typeName();
      ExternalDeclarationNode externalDeclaration();
      TUNode translationUnit();

      SubCompoundStatement compoundStatement(std::vector<ParameterNode> param = std::vector<ParameterNode>() );
      SubExpressionStatement expressionStatement();
  };
}

#endif
