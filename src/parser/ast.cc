#include "ast.h"
#include "pprinter.h"

#define PRETTY_PRINT(X) void X::prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel)
#define PPRINT(X)  do {pp.pprint((X), indentLevel);} while(0);
#define ADDINDENT()  do {indentLevel++;} while(0);
#define REMOVEINDENT()  do {indentLevel--;} while(0);


using namespace Parsing;

BinaryExpression::BinaryExpression(SubExpression lhs,
                                   SubExpression rhs,
                                   PunctuatorType op) :
  lhs(lhs),
  rhs(rhs),
  op(op)
{
}

PRETTY_PRINT(BinaryExpression)
{
  PPRINT('(');
  PPRINT(this->lhs);
  switch (this->op) {
    case PunctuatorType::ARROW:
    case PunctuatorType::MEMBER_ACCESS:
      PPRINT(op);
      break;
    default:
      PPRINT(' ');
      PPRINT(op);
      PPRINT(' ');
      break;
  }
  PPRINT(this->rhs);
  PPRINT(')');
}

UnaryExpression::UnaryExpression(PunctuatorType op, SubExpression operand) :
  operand(operand), op(op)
{
}

PRETTY_PRINT(UnaryExpression)
{
  PPRINT('(');
  PPRINT(this->op);
  PPRINT(this->operand);
  PPRINT(')');
}

VariableUsage::VariableUsage(std::string name) : name(name) {;}

PRETTY_PRINT(VariableUsage)
{
  PPRINT(this->name);
}

FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments)
        : funcName(funcName), arguments(arguments) {;}

PRETTY_PRINT(FunctionCall)
{
  PPRINT(funcName);
  PPRINT('(');
  if (!arguments.empty()) {
    auto size = arguments.size();
      for (auto argument : arguments) {
        PPRINT(argument);
        if (--size != 0) {
          PPRINT(',');
        }
      }
  }
  PPRINT(')');
}

TernaryExpression::TernaryExpression(SubExpression condition, 
                                     SubExpression lhs, 
                                     SubExpression rhs)
     : condition(condition), lhs(lhs), rhs(rhs)
{
 //TODO: type checking
}

PRETTY_PRINT(TernaryExpression)
{
  PPRINT('(');
  PPRINT(this->condition);
  PPRINT(std::string(" ? "));
  PPRINT(this->lhs);
  PPRINT(std::string(" : "));
  PPRINT(this->rhs);
  PPRINT(')');
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

PRETTY_PRINT(BasicType)
{
  switch(type) {
    case INT:
      PPRINT(std::string("int"));
      PPRINT(' ');
      break;
    case CHAR:
      PPRINT(std::string("char"));
      PPRINT(' ');
      break;
    default :
      /*case VOID: */
      PPRINT(std::string("void"));
      PPRINT(' ');
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
  PPRINT(std::string("struct "));

  if (name.length() > 0) {
    PPRINT(name);
    PPRINT(' ');
  }

  PPRINT('{');
  ADDINDENT();
  PPRINT('\n');
  
  for (auto typeSubDeclarationPair : content) {
    PPRINT(typeSubDeclarationPair.first);
    for (auto subDeclarationPair : typeSubDeclarationPair.second) {
      PPRINT(subDeclarationPair.first);
      if (subDeclarationPair.second) {
        PPRINT(' '); //TODO: this seems wrong
        PPRINT(subDeclarationPair.second);
      }
      PPRINT(';'); // <- FIXME: no idea if this belongs here, but probably correct
    }
  }
  REMOVEINDENT();
  PPRINT('\n');
  PPRINT('}');
}

CompoundStatement::CompoundStatement(std::vector<BlockItem> subStatements)
  : subStatements(std::move(subStatements))
{

}

PRETTY_PRINT(CompoundStatement) {
  // TODO: unfinished, add special case for last statement regarding newline
  PPRINT('{');
  ADDINDENT();
  PPRINT('\n');
  auto statement = this->subStatements.begin();
  auto afterLastStatement = this->subStatements.end();
  while (statement != afterLastStatement) { 
    PPRINT(*statement);
    auto next = ++statement;
    if (next != afterLastStatement) {
      PPRINT('\n');
    }
    statement = next;
  }
  REMOVEINDENT();
  PPRINT('\n');
  PPRINT('}');
}

PRETTY_PRINT(Pointer) {
  for(int n=0; n<counter; n++) {
    PPRINT('*');
  }
}

PRETTY_PRINT(ExpressionStatement) {
  // TODO : check whether this is working
  if (expression != NULL) {
    PPRINT(expression);
  }
  PPRINT(';');
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
  PPRINT(std::string("if ("));
  PPRINT(expression);
  PPRINT(')');
  ADDINDENT();
  PPRINT('\n');
  PPRINT(ifStatement);
  REMOVEINDENT();
  PPRINT('\n');

  if (elseStatement) {
    PPRINT(std::string("else "));
    PPRINT(elseStatement);
  }
}

PRETTY_PRINT(GotoStatement) {
  PPRINT(std::string("goto "));
  PPRINT(label);
  PPRINT(';');
}

PRETTY_PRINT(ContinueStatement) {
  PPRINT(std::string("continue;"));
}

PRETTY_PRINT(BreakStatement) {
  PPRINT(std::string("break;"));
}

PRETTY_PRINT(ReturnStatement) {
  PPRINT(std::string("return "));
  if (expression != NULL) {
    PPRINT(expression);
  }
  PPRINT(';');
}

PRETTY_PRINT(IterationStatement) {
  if (kind == WHILE) {
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string(")")); //FIXME: <- whitespace is required here, but only if this is followed by {
    PPRINT(statement);
  } else { // kind == DO
    PPRINT(std::string("do "));
    PPRINT(statement);
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string("));"));
  }
}

PRETTY_PRINT(LabeledStatement) {
  PPRINT(name);
  PPRINT(std::string(": "));
  PPRINT(statement);
}

PRETTY_PRINT(IdentifierList) {
  PPRINT(nameList[0]);
  for(int n = 1; n < (int) nameList.size(); n++) {
    PPRINT(',');
    PPRINT(nameList[n]);
  }
}

PRETTY_PRINT(Declarator)
{
  // TODO : unfinished, probably also broken
  for (auto i = this->pointerCounter; i>0; --i) {
    if (i>0)
    {
      PPRINT('(');
    }
    PPRINT('*');
  }
  PPRINT(this->directDeclarator);
  for (auto i = this->pointerCounter; i>1; --i) {
    PPRINT(')');
  }
}

Declaration::Declaration(TypeNode t, SubDeclarator declarator)
  :type(t),declarator(declarator){}

Declaration::Declaration(TypeNode t)
  :type(t){}

PRETTY_PRINT(Declaration)
{
  PPRINT(type);
  PPRINT(' ');
  if (declarator) {
    PPRINT(declarator);
  }
  PPRINT(';');
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
    PPRINT(externalDeclaration);
    PPRINT('\n');
  }
}

PRETTY_PRINT(ExternalDeclaration) {
  /*TODO: unfinished */
  PPRINT(this->type);
  if (this->declarator) {
    PPRINT(this->declarator);
    if (this->compoundStatement) {
      PPRINT(this->compoundStatement);
      return; // so that we don't print a semicolon
    }
  }
  PPRINT(';'); // declarations end with an ;
  PPRINT('\n');
}

PRETTY_PRINT(IdentifierDirectDeclarator) {
  /*TODO: unfinished*/
  PPRINT(this->identifier);
  if (help) {
    PPRINT(help);
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
  PPRINT(type);
  if (declarator) {
    PPRINT(declarator);
  }
}


PRETTY_PRINT(DeclaratorDirectDeclarator)
{
  /*TODO: unfinished*/
  PPRINT(declarator);
  if (help) {
    PPRINT(help);
  }
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
      PPRINT(idList);
    case EMPTYLIST:
      break;
    case PARAMETERLIST:
      PPRINT('(');
      auto size = paramList.size();
      for (auto parameter: paramList) {
        PPRINT(parameter);
        size--;
        if (size > 0) {
          PPRINT(std::string(", "));
        }
      }
      PPRINT(')');
  }
  if (help) {
    PPRINT('(');
    PPRINT(help);
    PPRINT(')');
  }
}  

SizeOfExpression::SizeOfExpression(std::pair<TypeNode, SubDeclarator> operand)
  : operand(operand) 
{
}

PRETTY_PRINT(SizeOfExpression)
{
  PPRINT(std::string("sizeof "));
  PPRINT('(');
  PPRINT(operand.first);
  if (operand.second) {
    PPRINT(operand.second);
  }
  PPRINT(')');
}
#undef PPRINT
#undef ADDINDENT
#undef REMOVEINDENT
#undef PRETTY_PRINT
