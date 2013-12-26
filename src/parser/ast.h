#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <typeinfo>
#include "../lexer/punctuatortype.h"
#include "../pos.h"
#include "pprinter.h"
#include "semantic.h"
#include "astNode.h"
#include "expressionNode.h"
#include "statementNode.h"
#include "typeNode.h"
#include "declaratorNode.h"

namespace Parsing {

class ASTNODE(Declaration) {
  public:
    Declaration(TypeNode t, SubDeclarator declarator, 
                Pos pos, shared_ptr<SemanticTree> semanticTree);
    Declaration(TypeNode t, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
    shared_ptr<SemanticTree> semanticTree;
};

typedef std::shared_ptr<Declaration> DeclarationNode;

class ASTNODE(ExternalDeclaration) {
  public:
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        );
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        );
    ExternalDeclaration(TypeNode type, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
    SubCompoundStatement compoundStatement;
    shared_ptr<SemanticTree> semanticTree;
};

typedef std::shared_ptr<ExternalDeclaration> ExternalDeclarationNode;

class ASTNODE(TranslationUnit) {
  public:
    TranslationUnit(std::vector<ExternalDeclarationNode> externalDeclarations, 
        Pos pos);
    PPRINTABLE
  private:
    std::vector<ExternalDeclarationNode> externalDeclarations;
};

typedef std::shared_ptr<TranslationUnit> TUNode;

class ASTNODE(Parameter) {
  public:
    Parameter(TypeNode type, SubDeclarator declarator, Pos pos);
    Parameter(TypeNode type, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
};

typedef std::shared_ptr<Parameter> ParameterNode;

typedef std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> StructContent;

class TYPE(StructType) {
  // TODO Add content
  public:
    StructType(Pos pos);
    StructType(std::string name, Pos pos);
    StructType(std::string name, StructContent content, Pos pos);
    bool canBeInFunctionDeclaration() { return !hasDeclaration; };
    
    // TODO : only calculate once
    virtual string getIdentifier() {
      return "struct_" + name;
    }

    string toString() {
      return name;
    }

    PPRINTABLE

  private:
    std::string name;
    StructContent content; 
    bool hasDeclaration;
};

typedef std::shared_ptr<StructType> StructNode;

class EXPRESSION(SizeOfExpression)
{
  public:
    SizeOfExpression(std::pair<TypeNode, SubDeclarator>, Pos pos);
    PPRINTABLE
  private:
    std::pair<TypeNode, SubDeclarator> operand;
};

}

#undef PPRINTABLE
#endif
