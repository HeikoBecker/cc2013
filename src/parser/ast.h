#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include "../lexer/punctuatortype.h"
#include "pprinter.h"


#define IS_ASTNODE : public AstNode

namespace Parser {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(PrettyPrinter &) {};
};

typedef std::shared_ptr<AstNode> AstChild;

class BinaryExpression IS_ASTNODE
{
  public:
    BinaryExpression(std::shared_ptr<Parser::AstNode> lhs,
                     std::shared_ptr<Parser::AstNode> rhs,
                     PunctuatorType op);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    AstChild lhs;
    AstChild rhs;
    PunctuatorType op;
};

class UnaryExpression IS_ASTNODE
{
 public:
   UnaryExpression(std::shared_ptr<Parser::AstNode> operand,
                   PunctuatorType op);
   void prettyPrint(PrettyPrinter & pp) override;
 private:
   AstChild operand;
   PunctuatorType op;
};

}
#endif
