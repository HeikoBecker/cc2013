#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <string>

namespace Parser {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual std::string prettyPrint() = delete;
};

}
#endif
