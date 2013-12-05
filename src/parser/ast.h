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

namespace Parsing {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(PrettyPrinter &) {};
};

typedef std::shared_ptr<AstNode> AstChild;

class ASTNODE(BinaryExpression)
{
  public:
    BinaryExpression(std::shared_ptr<Parsing::AstNode> lhs,
                     std::shared_ptr<Parsing::AstNode> rhs,
                     PunctuatorType op);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    AstChild lhs;
    AstChild rhs;
    PunctuatorType op;
};

class ASTNODE(UnaryExpression)
{
  public:
   UnaryExpression(std::shared_ptr<Parsing::AstNode> operand,
                   PunctuatorType op);
   void prettyPrint(PrettyPrinter & pp) override;
  private:
   AstChild operand;
   PunctuatorType op;
};

class ASTNODE(Variable)
{
  public:
    Variable(std::string name);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::string name;
};

class ASTNODE(FunctionCall)
{
  public:
    FunctionCall(std::shared_ptr<Variable> funcName,
                 std::vector<AstChild> arguments);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::shared_ptr<Variable> funcName;
    std::vector<AstChild> arguments;
};

}
#endif
