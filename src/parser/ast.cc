#include "ast.h"
#include "pprinter.h"

#define PRETTY_PRINT(X) void X::prettyPrint(PrettyPrinter & pp)

using namespace Parsing;

BinaryExpression::BinaryExpression(SubExpression lhs,
                                   SubExpression rhs,
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

UnaryExpression::UnaryExpression(PunctuatorType op, SubExpression operand) :
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

VariableUsage::VariableUsage(std::string name) : name(name) {;}

void VariableUsage::prettyPrint(PrettyPrinter & pp)
{
  pp.pprint(this->name);
}

FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments)
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

TernaryExpression::TernaryExpression(SubExpression condition, 
                                     SubExpression lhs, 
                                     SubExpression rhs)
     : condition(condition), lhs(lhs), rhs(rhs)
{
 //TODO: type checking
}

void TernaryExpression::prettyPrint(PrettyPrinter & pp) {
  pp.pprint('(');
  pp.pprint(this->condition);
  pp.pprint(std::string(" ? "));
  pp.pprint(this->lhs);
  pp.pprint(std::string(" : "));
  pp.pprint(this->rhs);
  pp.pprint(')');
}

BasicType::BasicType(std::string type) {
  
  if (type == "int") {
    type = INT;
  } else if (type == "char") {
    type = CHAR;
  } else if (type == "void") {
    type = VOID;
  } else {
    throw type + " is not a Basic Type";
  }
}

void BasicType::prettyPrint(PrettyPrinter & pp) {
  switch(type) {
    case INT: pp.pprint(std::string("int")); break;
    case CHAR: pp.pprint(std::string("char")); break;
    default : /*case VOID: */ pp.pprint(std::string("void")); break;
  }
}

StructType::StructType() {
  // default string is empty
  name = std::string("");
}

StructType::StructType(std::string str) {
  name = str;
}

PRETTY_PRINT(StructType) {
  pp.pprint(std::string("struct"));

  if (name.length() > 0) {
    pp.pprint(name);
  }

  pp.pprint('{');
  
  // TODO : show content
  pp.pprint('}');
}


CompoundStatement::CompoundStatement() {
  // TODO add block items  
  
}

PRETTY_PRINT(CompoundStatement) {

  pp.pprint('{');

  // TODO print children here
  pp.pprint('}');
}

Pointer::Pointer(int num) : counter(num) {}

PRETTY_PRINT(Pointer) {
  for(int n=0; n<counter; n++) {
    pp.pprint('*');
  }
}

ExpressionStatement::ExpressionStatement(Expression ex) {
  expression = ex;
}

PRETTY_PRINT(ExpressionStatement) {
  // TODO : check whether this is working
  expression.prettyPrint(pp); 
  pp.pprint(';');
}

