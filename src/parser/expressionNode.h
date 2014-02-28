#ifndef PARSER_EXPRESSION_H
#define PARSER_EXPRESSION_H
#pragma once

#include "astNode.h"
#include "semadecl.h"
#include "../codegen/cogen.h"

namespace llvm {
  class Value;
}

namespace Codegeneration {
  class IRCreator;
}

namespace Parsing {

class ASTNODE(Expression) 
{
  protected:
    SemanticDeclarationNode type;
    bool m_can_be_lvalue = false;
    CONS_INTER(Expression)
  public:
    virtual void checkSemanticConstraints() {};
    virtual SemanticDeclarationNode getType() {return this->type;};
    virtual bool can_be_lvalue() {return m_can_be_lvalue;};
    virtual llvm::Value* emit_rvalue(Codegeneration::IRCreator*);
    virtual llvm::Value* emit_lvalue(Codegeneration::IRCreator *);
    IR_EMITTING
};

typedef std::shared_ptr<Expression> SubExpression;

}

#endif
