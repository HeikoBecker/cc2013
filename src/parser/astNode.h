#ifndef PARSER_ASTNODE_H
#define PARSER_ASTNODE_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <typeinfo>
#include "../lexer/punctuatortype.h"
#include "../pos.h"
#include "pprinter.h"
#include "semantic.h"

/* This macro allows an easy switching of pprint in all methods*/
#define PPRINTABLE  void prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel = 0) override;

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
      virtual void prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel) {
        pp.pprint(std::string("\nIMPLEMENTATION MISSING!\n"), indentLevel);
        pp.pprint(std::string(typeid(*this).name()), indentLevel);
        pp.pprint('\n', indentLevel);
      };
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
