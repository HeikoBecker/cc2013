#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include "../lexer/punctuatortype.h"
#include "pprinter.h"

/**
 * This macro is meant to simplify a later transition from virtual inheritance
 * to CRTP static inheritance
 */
#define ASTNODE(X) X : public AstNode
#define EXPRESSION(X) X : public Expression
#define TYPE(X) X : public Type
#define STATEMENT(X) X : public Statement
#define JUMPSTATEMENT(X) X: public JumpStatement
#define ITERATIONSTATEMENT(X) X: public IterationStatement

namespace Parsing {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(PrettyPrinter &) {};
};

class ASTNODE(Expression) { };

typedef std::shared_ptr<AstNode> AstChild;
typedef std::shared_ptr<Expression> SubExpression;

class EXPRESSION(BinaryExpression)
{
  public:
    BinaryExpression(SubExpression lhs,
                     SubExpression rhs,
                     PunctuatorType op);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression lhs;
    SubExpression rhs;
    PunctuatorType op;
};

class EXPRESSION(UnaryExpression)
{
  public:
   UnaryExpression(PunctuatorType op,
       SubExpression operand);
   void prettyPrint(PrettyPrinter & pp) override;
  private:
   SubExpression operand;
   PunctuatorType op;
};

class EXPRESSION(VariableUsage)
{
  public:
    VariableUsage(std::string name);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::string name;
};

class EXPRESSION(FunctionCall)
{
  public:
    FunctionCall(SubExpression funcName,
                 std::vector<SubExpression> arguments);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression funcName;
    std::vector<SubExpression> arguments;
};

class EXPRESSION(TernaryExpression)
{
  public:
    TernaryExpression(SubExpression condition,
                      SubExpression lhs, 
                      SubExpression rhs);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression condition;
    SubExpression lhs;
    SubExpression rhs;
};

class ASTNODE(Type) {};

class TYPE(BasicType) {
  // this type includes int/char/void  
  public:
    BasicType(std::string str);
    void prettyPrint(PrettyPrinter & pp) override;
  
  private:
    enum ReturnType {
      VOID,
      INT,
      CHAR
    };

    ReturnType type;
};

class TYPE(StructType) {
  // TODO Add content
  public:
    StructType();
    StructType(std::string str);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::string name;
};

class ASTNODE(Statement) {};

class STATEMENT(CompoundStatement) {
  public:
    CompoundStatement();
    void prettyPrint(PrettyPrinter & pp) override;
};

class STATEMENT(ExpressionStatement) {
  public:
    ExpressionStatement(SubExpression ex);
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    SubExpression expression;
};

class ASTNODE(Pointer) {
  public:
    Pointer(int counter);
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    int counter;
};

class STATEMENT(SelectionStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    SelectionStatement(SubExpression ex, Statement ifStatement);
    SelectionStatement(
      SubExpression ex, 
      Statement ifStatement, 
      Statement elseStatement
    );

  // TODO : get rid of hasElseStatement ?
  private:
    SubExpression expression;
    Statement ifStatement;
    Statement elseStatement;
    bool hasElseStatement;
};

class STATEMENT(JumpStatement) { };

class JUMPSTATEMENT(GotoStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    GotoStatement(std::string label) : label(label) {};

  private:
    std::string label;
};

class JUMPSTATEMENT(ContinueStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    ContinueStatement() { };
};

class JUMPSTATEMENT(BreakStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    BreakStatement() { };
};

class JUMPSTATEMENT(ReturnStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    ReturnStatement() { };
    ReturnStatement(SubExpression ex) :expression(ex) { };
  
  private:
    SubExpression expression;
};

// iteration statement i.e. while and for
enum IterationEnum {
  WHILE,
  DOWHILE
};

typedef std::shared_ptr<JumpStatement> SubJumpStatement;
typedef std::shared_ptr<CompoundStatement> SubCompoundStatement;
typedef std::shared_ptr<Statement> SubStatement;

class STATEMENT(IterationStatement) { 
  public:
    IterationStatement(SubExpression ex, SubStatement st, IterationEnum k): expression(ex), statement(st), kind(k) { };
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    SubExpression expression;
    SubStatement statement;
    IterationEnum kind;
};

}
#endif
