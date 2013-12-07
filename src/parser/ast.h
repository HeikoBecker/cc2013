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

namespace Parsing {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(PrettyPrinter &) {};
};

class ASTNODE(Expression) {};

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

}
#endif
