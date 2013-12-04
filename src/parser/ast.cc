#include "ast.h"

using namespace Parser;

BinaryExpression::BinaryExpression(std::shared_ptr<AstNode> lhs,
                                   std::shared_ptr<AstNode> rhs,
                                   PunctuatorType op) :
  lhs(lhs),
  rhs(rhs),
  op(op)
{
}

void BinaryExpression::prettyPrint(std::ostream & out) {
  pprint('(', out);
  pprint(this->lhs, out);
  pprint(op, out);
  pprint(this->rhs, out);
  pprint(')', out);
}
