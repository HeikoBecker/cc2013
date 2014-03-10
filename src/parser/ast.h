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
#include "../codegen/cogen.h"

#define EMIT_RVALUE llvm::Value* emit_rvalue(Codegeneration::IRCreator *) override;
#define EMIT_LVALUE llvm::Value* emit_lvalue(Codegeneration::IRCreator *) override;

namespace Codegeneration {
  class IRCreator;
}

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
    EMIT_RVALUE
    EMIT_LVALUE
    void emit_condition(
        Codegeneration::IRCreator* creator,
        llvm::BasicBlock* trueSuccessor,
        llvm::BasicBlock* falseSuccessor
    );
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
    void emit_condition(
        Codegeneration::IRCreator* creator,
        llvm::BasicBlock* trueSuccessor,
        llvm::BasicBlock* falseSuccessor
    );

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
    const std::string name;
  private:
    SemanticTreeNode semanticTree;
};

class EXPRESSION(Literal)
{
  public:
    Literal(std::string name, Pos pos);
    PPRINTABLE
    EMIT_LVALUE
    EMIT_RVALUE
    std::string unescaped;

  private:
    std::string name;
};

class EXPRESSION(Constant)
{
  public:
    Constant(std::string name, Pos pos, Lexing::ConstantType ct);
    PPRINTABLE
    EMIT_RVALUE
    void setType(SemanticDeclarationNode s);

  private:
    Lexing::ConstantType ct;
    std::string name;
    std::string unescaped;
};

class EXPRESSION(FunctionCall)
{
  public:
    FunctionCall(SubExpression funcName,
                 std::vector<SubExpression> arguments,
                 Pos pos);
    PPRINTABLE
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
    SemanticDeclarationNode getSemanticNode();

    IR_EMITTING
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

    virtual bool isFunction();

    SemanticDeclarationNode getSemanticNode();


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
    bool isFunction() override; 

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

    bool isStruct() override;

    bool containsDeclaration() override;
    
    // TODO : only calculate once
    virtual std::string getIdentifier();
    std::string toString() override;

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
    SemanticDeclarationNode sem_type;
  private:
    std::pair<TypeNode, SubDeclarator> operand;
};

}

#undef PPRINTABLE
#endif
