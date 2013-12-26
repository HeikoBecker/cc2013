#ifndef PARSER_EXPRESSION_H
#define PARSER_EXPRESSION_H
#pragma once

#include "astNode.h"

namespace Parsing {

class ASTNODE(Expression) 
{
    CONS_INTER(Expression)

};

typedef std::shared_ptr<Expression> SubExpression;

}

#endif
