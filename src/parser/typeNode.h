#ifndef PARSER_TYPE_H
#define PARSER_TYPE_H
#pragma once

#include "astNode.h"
#include "string.h"

namespace Parsing {

class ASTNODE(Type)
{
  CONS_INTER(Type)

  public:
    virtual bool canBeInFunctionDeclaration() { return true; };
    virtual std::string toString() { return "unknown"; }
};

typedef std::shared_ptr<Type> TypeNode;

class TYPE(BasicType) {
  // this type includes int/char/void  
  public:
    BasicType(std::string str, Pos pos);
    std::string toString();
    PPRINTABLE
  
  private:
    enum ReturnType {
      VOID,
      INT,
      CHAR
    };

    ReturnType type;
};

}

#endif