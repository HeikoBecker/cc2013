#ifndef PARSER_ASTNODE_H
#define PARSER_ASTNODE_H
#pragma once
#include <memory>
#include "../lexer/punctuatortype.h"
#include "../utils/pos.h"
#include "semantic.h"

/* This macro allows an easy switching of pprint in all methods*/
#define PPRINTABLE  void prettyPrint(unsigned int indentLevel = 0) override;

/* This macro is used in intermediate classes */
#define CONS_INTER(X) protected : X(Pos pos) : AstNode(pos){};

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
#define DIRECTDECLARATOR(X) X : public DirectDeclarator

namespace Parsing {
  class AstNode
  {
    protected:
      AstNode(Pos pos) : m_pos(std::move(pos)) {};
    public:
      virtual ~AstNode() {};
      virtual void prettyPrint(unsigned int) {};
      Pos inline pos() {return m_pos;}
    private:
      Pos m_pos;
  };


typedef std::shared_ptr<AstNode> AstChild;
typedef std::shared_ptr<AstNode> AstRoot;
/* TODO: this shouldn't be so general */
typedef std::shared_ptr<AstNode> BlockItem;

}

#endif
