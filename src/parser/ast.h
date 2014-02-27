#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include "../lexer/punctuatortype.h"
#include "../utils/pos.h"
#include "semantic.h"
#include "astNode.h"
#include "expressionNode.h"
#include "statementNode.h"
#include "typeNode.h"
#include "declaratorNode.h"
#include "expressionNode.h"

#include "../lexer/lexer.h"

#define EMIT_RVALUE llvm::Value* emit_rvalue(llvm::Module &) override;
#define EMIT_LVALUE llvm::Value* emit_lvalue(llvm::Module &) override;


namespace Parsing {

typedef std::shared_ptr<SemanticTree> SemanticTreeNode;

class EXPRESSION(BinaryExpression)
{
  public:
    BinaryExpression(SubExpression lhs,
                     SubExpression rhs,
                     PunctuatorType op,
                     Pos pos,
                     const Pos* const operator_position = nullptr);
    PPRINTABLE
    IR_EMITTING
    EMIT_RVALUE
  private:
    SubExpression lhs;
    SubExpression rhs;
    PunctuatorType op;
};

class EXPRESSION(UnaryExpression)
{
  public:
   UnaryExpression(PunctuatorType op,
       SubExpression operand,
       Pos pos);
  PPRINTABLE
  EMIT_LVALUE
  EMIT_RVALUE

  private:
   SubExpression operand;
   PunctuatorType op;
};

class EXPRESSION(VariableUsage)
{
  public:
    VariableUsage(std::string name, Pos pos, SemanticTreeNode semanticTree);
    SemanticDeclarationNode getType() override;
    void checkSemanticConstraints() override;
    // maps a variable name to its type when it is used in the context of a
    // struct
    SemanticDeclarationNode getType(SubSemanticNode structContext);
   PPRINTABLE
   EMIT_LVALUE
   EMIT_RVALUE

  private:
    std::string name;
    SemanticTreeNode semanticTree;
};

class EXPRESSION(Literal)
{
  public:
    Literal(std::string name, Pos pos);
    PPRINTABLE
    EMIT_LVALUE
    EMIT_RVALUE

  private:
    std::string name;
};

class EXPRESSION(Constant)
{
  public:
    Constant(std::string name, Pos pos, Lexing::ConstantType ct);
    PPRINTABLE
    EMIT_RVALUE

  private:
    Lexing::ConstantType ct;
    std::string name;
};

class EXPRESSION(FunctionCall)
{
  public:
    FunctionCall(SubExpression funcName,
                 std::vector<SubExpression> arguments,
                 Pos pos);
    PPRINTABLE
    EMIT_LVALUE
    EMIT_RVALUE

  private:
    SubExpression funcName;
    std::vector<SubExpression> arguments;
};

class EXPRESSION(TernaryExpression)
{
  public:
    TernaryExpression(SubExpression condition,
                      SubExpression lhs, 
                      SubExpression rhs,
                      Pos);
    PPRINTABLE
    EMIT_LVALUE
    EMIT_RVALUE
    
  private:
    SubExpression condition;
    SubExpression lhs;
    SubExpression rhs;
};

class ASTNODE(Declaration) {
  public:
    Declaration(TypeNode t, SubDeclarator declarator, 
                Pos pos, SemanticTreeNode semanticTree);
    Declaration(TypeNode t, Pos pos);
    SemanticDeclarationNode getSemanticNode() { return declNode; }

    PPRINTABLE
  private:
    TypeNode type;

    SubDeclarator declarator;
    SemanticTreeNode semanticTree;
    SemanticDeclarationNode declNode;
};

typedef std::shared_ptr<Declaration> DeclarationNode;
class ASTNODE(ExternalDeclaration) {
  public:
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        Pos pos,
                        SemanticTreeNode semanticTree,
                        bool assign = true
                        );
    ExternalDeclaration(TypeNode type, Pos pos,
                        SemanticTreeNode semanticTree);

    virtual bool isFunction() { return false; }

    SemanticDeclarationNode getSemanticNode() { return declNode; }


    PPRINTABLE
    IR_EMITTING
  protected:
    TypeNode type;
    SubDeclarator declarator;
    SemanticTreeNode semanticTree;
    SemanticDeclarationNode declNode;
};

class FunctionDefinition : public ExternalDeclaration {
  public:
    FunctionDefinition(TypeNode type,
        SubDeclarator declarator,
        SubCompoundStatement compoundStatement,
        Pos pos,
        SemanticTreeNode semanticTree
        );
    PPRINTABLE
    IR_EMITTING
    bool isFunction() { return true;}

  private:
    SubCompoundStatement compoundStatement;
};

typedef std::shared_ptr<ExternalDeclaration> ExternalDeclarationNode;

class ASTNODE(TranslationUnit) {
  public:
    TranslationUnit(std::vector<ExternalDeclarationNode> externalDeclarations, 
        Pos pos);
    PPRINTABLE
    IR_EMITTING
  private:
    std::vector<ExternalDeclarationNode> externalDeclarations;
};

typedef std::shared_ptr<TranslationUnit> TUNode;


typedef std::shared_ptr<Parameter> ParameterNode;

typedef std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> StructContent;

class TYPE(StructType) {
  // TODO Add content
  public:
    StructType(Pos pos);
    StructType(std::string name, Pos pos);
    StructType(std::string name, StructContent content, Pos pos);

    bool isStruct() {
      return true;
    }

    bool containsDeclaration() {
      return hasDeclaration;
    }
    
    // TODO : only calculate once
    virtual std::string getIdentifier() {
      return "struct_" + name;
    }

    std::string toString() {
      return name;
    }

    PPRINTABLE

  private:
    std::string name;
    StructContent content;
    std::vector<std::pair<TypeNode, SubDeclarator>> mycontent;
    bool hasDeclaration;
};

typedef std::shared_ptr<StructType> StructNode;

class EXPRESSION(SizeOfExpression)
{
  public:
    SizeOfExpression(std::pair<TypeNode, SubDeclarator>, Pos pos);
    PPRINTABLE
    EMIT_RVALUE
  private:
    std::pair<TypeNode, SubDeclarator> operand;
};

}

#undef PPRINTABLE
#endif
