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
    this->type = INT;
  } else if (type == "char") {
    this->type = CHAR;
  } else if (type == "void") {
    this->type = VOID;
  } else {
    throw type + " is not a Basic Type";
  }
}

void BasicType::prettyPrint(PrettyPrinter & pp) {
  switch(type) {
    case INT:
      pp.pprint(std::string("int"));
      pp.pprint(' ');
      break;
    case CHAR:
      pp.pprint(std::string("char"));
      pp.pprint(' ');
      break;
    default :
      /*case VOID: */
      pp.pprint(std::string("void"));
      pp.pprint(' ');
      break;
  }
}

StructType::StructType(std::string name) : name(name) {
  // default string is empty
  content = StructContent();
}


StructType::StructType() {
  // default string is empty
  name = std::string("");
  content = StructContent();
}

StructType::StructType(std::string name, StructContent content) : name(name), content(content) {
}

PRETTY_PRINT(StructType) {
  pp.pprint(std::string("struct"));

  if (name.length() > 0) {
    pp.pprint(name);
  }

  pp.pprint('{');
  pp.pprint('\n');
  
  for (auto typeSubDeclarationPair : content) {
    pp.pprint(typeSubDeclarationPair.first);
    for (auto subDeclarationPair : typeSubDeclarationPair.second) {
      pp.pprint(subDeclarationPair.first);
      if (subDeclarationPair.second) {
        pp.pprint(' '); //TODO: this seems wrong
        pp.pprint(subDeclarationPair.second);
      }
    }
  }
  pp.pprint('\n');
  pp.pprint('}');
}

CompoundStatement::CompoundStatement(std::vector<BlockItem> subStatements)
  : subStatements(std::move(subStatements))
{

}

PRETTY_PRINT(CompoundStatement) {

  pp.pprint('{');
  pp.addIndentLevel();
  for (auto statement : this->subStatements) {
    pp.pprint(statement);
    pp.pprint('\n');
  }
  pp.removeIndentLevel();
  pp.pprint('}');
}

PRETTY_PRINT(Pointer) {
  for(int n=0; n<counter; n++) {
    pp.pprint('*');
  }
}

PRETTY_PRINT(ExpressionStatement) {
  // TODO : check whether this is working
  if (expression != NULL) {
    pp.pprint(expression);
  }
  pp.pprint(';');
}

SelectionStatement::SelectionStatement(SubExpression ex, SubStatement ifStat) {
  expression = ex;
  ifStatement = ifStat;
}

SelectionStatement::SelectionStatement(
  SubExpression ex, 
  SubStatement ifStat, 
  SubStatement elseStat) {
  expression = ex;
  ifStatement = ifStat;
  elseStatement = elseStat;
}


PRETTY_PRINT(SelectionStatement) {
  pp.pprint(std::string("if ("));
  pp.pprint(expression);
  pp.pprint(')');
  pp.addIndentLevel();
  pp.pprint('\n');
  pp.pprint(ifStatement);
  pp.removeIndentLevel();
  pp.pprint('\n');

  if (elseStatement) {
    pp.pprint(elseStatement);
  }
}

PRETTY_PRINT(GotoStatement) {
  pp.pprint(std::string("goto "));
  pp.pprint(label);
  pp.pprint(';');
}

PRETTY_PRINT(ContinueStatement) {
  pp.pprint(std::string("continue;"));
}

PRETTY_PRINT(BreakStatement) {
  pp.pprint(std::string("break;"));
}

PRETTY_PRINT(ReturnStatement) {
  pp.pprint(std::string("return "));
  if (expression != NULL) {
    pp.pprint(expression);
  }
  pp.pprint(';');
}

PRETTY_PRINT(IterationStatement) {
  if (kind == WHILE) {
    pp.pprint(std::string("while ("));
    pp.pprint(expression);
    pp.pprint(std::string("))"));
    pp.pprint(statement);
  } else { // kind == DO
    pp.pprint(std::string("do "));
    pp.pprint(statement);
    pp.pprint(std::string("while ("));
    pp.pprint(expression);
    pp.pprint(std::string("));"));
  }
}

PRETTY_PRINT(LabeledStatement) {
  pp.pprint(name);
  pp.pprint(std::string(": "));
  pp.pprint(statement);
}

PRETTY_PRINT(IdentifierList) {
  pp.pprint(nameList[0]);
  for(int n = 1; n < (int) nameList.size(); n++) {
    pp.pprint(',');
    pp.pprint(nameList[n]);
  }
}

PRETTY_PRINT(Declarator)
{
  // TODO : unfinished
  for (auto i = 0; i < this->pointerCounter; ++i) {
    pp.pprint('*');
  }
  pp.pprint(this->directDeclarator);
}

Declaration::Declaration(TypeNode t, SubDeclarator declarator)
  :type(t),declarator(declarator){}

Declaration::Declaration(TypeNode t)
  :type(t){}

PRETTY_PRINT(Declaration)
{
  pp.pprint(type);
  pp.pprint(' ');
  pp.pprint(declarator);
}


ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement)
  : type(type), declarator(declarator), compoundStatement(compoundStatement)
{}

ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator)
  : type(type), declarator(declarator)
{}

ExternalDeclaration::ExternalDeclaration(TypeNode type)
  : type(type)
{}


TranslationUnit::TranslationUnit(
    std::vector<ExternalDeclarationNode> externalDeclarations
    ) : externalDeclarations(externalDeclarations) {}

PRETTY_PRINT(TranslationUnit) {
  /*TODO: unfinished */
  for(auto externalDeclaration : externalDeclarations) {
    pp.pprint(externalDeclaration);
  }
}

PRETTY_PRINT(ExternalDeclaration) {
  /*TODO: unfinished */
  pp.pprint(this->type);
  if (this->declarator) {
    pp.pprint(this->declarator);
    if (this->compoundStatement) {
      pp.pprint(this->compoundStatement);
      return; // so that we don't print a semicolon
    }
  }
  pp.pprint(';'); // declarations end with an ;
  pp.pprint('\n');
}

PRETTY_PRINT(IdentifierDirectDeclarator) {
  /*TODO: unfinished*/
  pp.pprint(this->identifier);
  if (help) {
    pp.pprint(help);
  }
}

Parameter::Parameter(TypeNode type, SubDeclarator declarator)
  : type(type), declarator(declarator) {
  
}

Parameter::Parameter(TypeNode type)
  : type(type) {
  
}

PRETTY_PRINT(Parameter) {
  /*TODO: unfinished*/
  pp.pprint(type);
  pp.pprint(declarator);
}


PRETTY_PRINT(DeclaratorDirectDeclarator)
{
  /*TODO: unfinished*/
  pp.pprint(declarator);
  pp.pprint(help);
}



DirectDeclaratorHelp::DirectDeclaratorHelp() 
{
  helperType = EPSILON;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(
    SubDirectDeclaratorHelp help) : help(help)
{
  helperType = EMPTYLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(std::vector<ParameterNode> paramList)
  : paramList(paramList) 
{  
  helperType = PARAMETERLIST; 
}

DirectDeclaratorHelp::DirectDeclaratorHelp(std::vector<ParameterNode> paramList,
                                           SubDirectDeclaratorHelp help)
  :help(help), paramList(paramList) 
{
  helperType = PARAMETERLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(SubIdentifierList idList)
  : idList(idList) 
{
  helperType = IDENTIFIERLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(SubIdentifierList idList,
                                           SubDirectDeclaratorHelp help)
  : help(help), idList(idList)
{
  helperType = IDENTIFIERLIST;
}


PRETTY_PRINT(DirectDeclaratorHelp)
{
  switch (helperType) {
    case EPSILON:
      return;
    case IDENTIFIERLIST:
      pp.pprint(idList);
    case EMPTYLIST:
      break;
    case PARAMETERLIST:
      for (auto parameter: paramList) {
        pp.pprint(parameter);
        pp.pprint(", "); // TODO: don't print this for the last one
      }
  }
  if (help) {
    pp.pprint('(');
    pp.pprint(help);
    pp.pprint('(');
  }
}  
