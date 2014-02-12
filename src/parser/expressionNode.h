#ifndef PARSER_EXPRESSION_H
#define PARSER_EXPRESSION_H
#pragma once

#include "astNode.h"
#include "semadecl.h"

namespace Parsing {

class ASTNODE(Expression) 
{
  protected:
    SemanticDeclarationNode type;
  public:
    virtual void checkSemanticConstraints() {};
    virtual SemanticDeclarationNode getType() {return this->type;};
    CONS_INTER(Expression)
};

typedef std::shared_ptr<Expression> SubExpression;

}

#endif
