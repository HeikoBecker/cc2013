#include "ast.h"
#include "pprinter.h"

using namespace Parsing;

BinaryExpression::BinaryExpression(AstChild lhs,
                                   AstChild rhs,
                                   PunctuatorType op) :
  lhs(lhs),
  rhs(rhs),
  op(op)
{
}

void BinaryExpression::prettyPrint(PrettyPrinter & pp)
{
  pp.pprint('(');
  pp.pprint(this->lhs);
  pp.pprint(op);
  pp.pprint(this->rhs);
  pp.pprint(')');
}

UnaryExpression::UnaryExpression(PunctuatorType op, AstChild operand) :
  operand(operand), op(op)
{
}

void UnaryExpression::prettyPrint(PrettyPrinter & pp)
{
  pp.pprint(this->op);
  pp.pprint('(');
  pp.pprint(this->operand);
  pp.pprint(')');
}

Variable::Variable(std::string name) : name(name) {;}

void Variable::prettyPrint(PrettyPrinter & pp)
{
  pp.pprint(this->name);
}

FunctionCall::FunctionCall(AstChild funcName,
                           std::vector<AstChild> arguments)
        : funcName(funcName), arguments(arguments) {;}

void FunctionCall::prettyPrint(PrettyPrinter & pp)
{
  pp.pprint(funcName);
  pp.pprint('(');
  if (!arguments.empty()) {
    auto size = arguments.size();
      for (auto argument : arguments) {
        pp.pprint(argument);
        if (--size != 0) {
          pp.pprint(',');
        }
      }
  }
  pp.pprint(')');
}