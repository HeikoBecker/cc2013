#include "ast.h"
#include "pprinter.h"
#include "parser.h"

bool g_skipNewLineBeforeBlockStatement = false; // TODO: FIXME: global variables are BAD!
bool g_skipNewLineBeforeSelectionStatement = false; // TODO: FIXME: global variables are BAD!

#define PRETTY_PRINT(X) void X::prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel)
#ifdef DEBUG
/* Beware of macro magic
 * This prints the name of function calling PPRINT, it's argument and finally
 * any output produced by the function
 */
#define PPRINT(X)  do { \
  pp.pprint(std::string("\n")+__PRETTY_FUNCTION__+"\t: ", 0);\
  pp.pprint(std::string(#X) + ": ", 0);\
  pp.pprint((X), indentLevel);\
  pp.pprint('\n',0);\
} while(0);
#else
#define PPRINT(X)  do {pp.pprint((X), indentLevel);} while(0);
#endif
#define ADDINDENT()  do {indentLevel++;} while(0);
#define REMOVEINDENT()  do {indentLevel--;} while(0);
#define RESETINDENT() \
  auto reset = indentLevel;\
  indentLevel = 0;
#define RESTOREINDENT() indentLevel = reset;


using namespace Parsing;

BinaryExpression::BinaryExpression(SubExpression lhs,
                                   SubExpression rhs,
                                   PunctuatorType op,
                                   Pos pos) :
  Expression(pos),
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
    case PunctuatorType::ARRAY_ACCESS:
      PPRINT('[');
      PPRINT(this->rhs);
      PPRINT(']');
      PPRINT(')');
      return;
      //break;
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

UnaryExpression::UnaryExpression(PunctuatorType op, SubExpression operand, Pos pos) :
  Expression(pos), operand(operand), op(op)
{
}

PRETTY_PRINT(UnaryExpression)
{
  PPRINT('(');
  PPRINT(this->op);
  if (this->op == PunctuatorType::SIZEOF && !(std::dynamic_pointer_cast<SizeOfExpression>(operand))) {
    PPRINT(' ');
  }
  PPRINT(this->operand);
  PPRINT(')');
}

VariableUsage::VariableUsage(std::string name, Pos pos, 
                             SemanticTreeNode semanticTree) 
  : Expression(pos), name(name), semanticTree(semanticTree) {
    

#ifdef DEBUG
 cout <<"USAGE " + name<<endl;
#endif
 pair<TypeNode, int> p = semanticTree->lookUpType(name, pos);
#ifdef DEBUG
 cout<<"TYPE : "<<p.first->toString()<<" POINTER : "<<p.second<<endl;
#endif
}

Literal::Literal(std::string name, Pos pos)
  : Expression(pos), name(name) {}

Constant::Constant(std::string name, Pos pos)
  : Expression(pos), name(name) {}


PRETTY_PRINT(VariableUsage)
{
  PPRINT(this->name);
}

PRETTY_PRINT(Literal)
{
  PPRINT(this->name);
}

PRETTY_PRINT(Constant)
{
  PPRINT(this->name);
}

FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments, Pos pos)
        : Expression(pos), funcName(funcName), arguments(arguments) {;}

PRETTY_PRINT(FunctionCall)
{
  PPRINT('(');
  PPRINT(funcName);
  PPRINT('(');
  if (!arguments.empty()) {
    auto size = arguments.size();
      for (auto argument : arguments) {
        PPRINT(argument);
        if (--size != 0) {
          PPRINT(',');
          PPRINT(' ');
        }
      }
  }
  PPRINT(')');
  PPRINT(')');
}

TernaryExpression::TernaryExpression(SubExpression condition, 
                                     SubExpression lhs, 
                                     SubExpression rhs,
                                     Pos pos)
     : Expression(pos), condition(condition), lhs(lhs), rhs(rhs)
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

BasicType::BasicType(std::string type, Pos pos) : Type(pos)
{
  if (type == "int") {
    this->type = INT;
  } else if (type == "char") {
    this->type = CHAR;
  } else if (type == "void") {
    this->type = VOID;
  } else {
    throw ParsingException(type + " is not a Basic Type", pos);
  }
}

string BasicType::toString() {
  if (this->type == INT) {
    return "int";
  } else if (type == CHAR) {
    return "char";
  } else { // if (type == VOID) {
    return "void";
  }
}

PRETTY_PRINT(BasicType)
{
  switch(type) {
    case INT:
      PPRINT(std::string("int"));
      break;
    case CHAR:
      PPRINT(std::string("char"));
      break;
    default :
      /*case VOID: */
      PPRINT(std::string("void"));
      break;
  }
}

StructType::StructType(std::string name, Pos pos) : Type(pos), name(name) {
  // default string is empty
  content = StructContent();
  hasDeclaration = false;
}

StructType::StructType(Pos pos) : Type(pos) {
  // default string is empty
  name = std::string("");
  content = StructContent();
  hasDeclaration = false;
}

StructType::StructType(std::string name, StructContent content, Pos pos)
  : Type(pos), name(name), content(content) {
  hasDeclaration = true;
}

PRETTY_PRINT(StructType) {
  // TODO: this looks way too complicated
  PPRINT(std::string("struct "));

  if (name.length() > 0) {
    PPRINT(name);
  }

  if (!content.empty()) {
    PPRINT('\n');
    PPRINT('{');
    ADDINDENT();

    for (auto typeSubDeclarationPair : content) {
      PPRINT('\n');
      PPRINT(typeSubDeclarationPair.first);
      if (!typeSubDeclarationPair.second.empty()) {
        PPRINT(' '); // print space between type and following stuff
      } else {
        PPRINT(';');
      }
      for (auto subDeclarationPair : typeSubDeclarationPair.second) {
        PPRINT(subDeclarationPair.first);
        if (subDeclarationPair.second) {
          //PPRINT(' '); //TODO: this seems wrong
          PPRINT(subDeclarationPair.second);
        }
        PPRINT(';'); // <- FIXME: no idea if this belongs here, but probably correct
      }
    }
    REMOVEINDENT();
    PPRINT('\n');
    PPRINT('}');
  }
}

CompoundStatement::CompoundStatement(std::vector<BlockItem> subStatements, Pos pos)
  : Statement(pos), subStatements(std::move(subStatements))
{

}

PRETTY_PRINT(CompoundStatement) {
  // TODO: unfinished, add special case for last statement regarding newline
  if (!g_skipNewLineBeforeBlockStatement) {
    PPRINT('\n');
  } else {
    PPRINT(' ');
    g_skipNewLineBeforeBlockStatement = false; //FIXME: global variable!
  }
  PPRINT('{');
  ADDINDENT();
  for (auto statement : subStatements) {
    PPRINT(statement);
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
  PPRINT('\n');
  if (expression != NULL) {
    PPRINT(expression);
  }
  PPRINT(';');
}

SelectionStatement::SelectionStatement(SubExpression ex,
    SubStatement ifStat,
    Pos pos) : Statement(pos)
{
  expression = ex;
  ifStatement = ifStat;
}

SelectionStatement::SelectionStatement(
  SubExpression ex, 
  SubStatement ifStat, 
  SubStatement elseStat,
  Pos pos) : Statement(pos)
{
  expression = ex;
  ifStatement = ifStat;
  elseStatement = elseStat;
}


PRETTY_PRINT(SelectionStatement) {
  auto suppressedIndent = false;
  if (g_skipNewLineBeforeSelectionStatement) {
    g_skipNewLineBeforeSelectionStatement = false;
    PPRINT(' ');
  } else {
    PPRINT('\n');
  }
  PPRINT(std::string("if ("));
  PPRINT(expression);
  PPRINT(')');
  if (std::dynamic_pointer_cast<CompoundStatement>(ifStatement)) {
    suppressedIndent = true;
    g_skipNewLineBeforeBlockStatement = true;
  }
  if (!suppressedIndent) {
    ADDINDENT();
  }
  PPRINT(ifStatement);
  if (!suppressedIndent) {
    REMOVEINDENT();
  }

  if (elseStatement) {
    if (suppressedIndent) {
      PPRINT(' ');
      suppressedIndent = false;
    } else {
      PPRINT('\n');
    }
    PPRINT(std::string("else"));
    // don't set a new indent level if we have a selection statement
    if (std::dynamic_pointer_cast<SelectionStatement>(elseStatement)) {
      g_skipNewLineBeforeSelectionStatement = true;
      suppressedIndent = true;
    } else if (std::dynamic_pointer_cast<CompoundStatement>(elseStatement)) {
      suppressedIndent = true;
      g_skipNewLineBeforeBlockStatement = true;
    } else {
      ADDINDENT();
    }
    PPRINT(elseStatement);
    if (!suppressedIndent) {
      REMOVEINDENT(); // only remove indentLevel if we have added one
    }
  }
}

PRETTY_PRINT(GotoStatement) {
  PPRINT('\n');
  PPRINT(std::string("goto "));
  PPRINT(label);
  PPRINT(';');
}

PRETTY_PRINT(ContinueStatement) {
  PPRINT('\n');
  PPRINT(std::string("continue;"));
}

PRETTY_PRINT(BreakStatement) {
  PPRINT('\n');
  PPRINT(std::string("break;"));
}

PRETTY_PRINT(ReturnStatement) {
  PPRINT('\n');
  PPRINT(std::string("return "));
  if (expression != NULL) {
    PPRINT(expression);
  }
  PPRINT(';');
}

PRETTY_PRINT(IterationStatement) {
  PPRINT('\n');
  if (kind == WHILE) {
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string(")"));
    if (std::dynamic_pointer_cast<CompoundStatement>(statement)) {
      g_skipNewLineBeforeBlockStatement = true;
      PPRINT(statement);
    } else {
      ADDINDENT();
      PPRINT(statement);
      REMOVEINDENT();
    }
  } else { // kind == DO
    PPRINT(std::string("do "));
    PPRINT(statement);
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string(");"));
  }
}

PRETTY_PRINT(LabeledStatement) {
  RESETINDENT();
  PPRINT('\n');
  PPRINT(name);
  PPRINT(std::string(":"));
  RESTOREINDENT();
  if (std::dynamic_pointer_cast<LabeledStatement>(statement)) {
    PPRINT(statement);
  } else {
    PPRINT(statement);
  }
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
    PPRINT('(');
    PPRINT('*');
  }
  if (directDeclarator) {
    PPRINT(this->directDeclarator);
  }
  for (auto i = this->pointerCounter; i>0; --i) {
    PPRINT(')');
  }
}

Declaration::Declaration(TypeNode t, SubDeclarator declarator, Pos pos, shared_ptr<SemanticTree> semanticTree)
  : AstNode(pos), type(t),declarator(declarator), semanticTree(semanticTree) {
    semanticTree->addDeclaration(type, declarator, pos);
}


Declaration::Declaration(TypeNode t, Pos pos)
  : AstNode(pos), type(t){}

PRETTY_PRINT(Declaration)
{
  PPRINT('\n');
  PPRINT(type);
  PPRINT(' ');
  if (declarator) {
    PPRINT(declarator);
  }
  PPRINT(';');
}


ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        )
  : AstNode(pos), type(type), declarator(declarator),
    compoundStatement(compoundStatement), semanticTree(semanticTree)
{
    semanticTree->addDeclaration(type, declarator, pos);
}

ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        )
  : AstNode(pos), type(type), declarator(declarator),
    semanticTree(semanticTree)
{
  // TODO : I dont think we need that anymore ..
  if (semanticTree) {
    semanticTree->addDeclaration(type, declarator, pos);
  }
  
}

ExternalDeclaration::ExternalDeclaration(TypeNode type, Pos pos)
  : AstNode(pos), type(type)
{
}


TranslationUnit::TranslationUnit(
    std::vector<ExternalDeclarationNode> externalDeclarations,
    Pos pos
    ) : AstNode(pos), externalDeclarations(externalDeclarations) {}

PRETTY_PRINT(TranslationUnit) {
  auto size = externalDeclarations.size();
  for(auto externalDeclaration : externalDeclarations) {
    PPRINT(externalDeclaration);
    if (size > 1) {
      PPRINT('\n');
      size--;
    }
  }
}

PRETTY_PRINT(ExternalDeclaration) {
  /*TODO: unfinished */
  PPRINT(this->type);
  if (this->declarator) {
    PPRINT(' ');
    if (compoundStatement) {
      PPRINT('('); // <- WTF, that's a really coding style
    }
    PPRINT(this->declarator);
    if (compoundStatement) {
      PPRINT(')');
    }
    if (this->compoundStatement) {
      PPRINT(this->compoundStatement);
      PPRINT('\n');
#ifdef DEBUG
      PPRINT(std::string("ExternalDeclaration END"));
#endif
      return; // so that we don't print a semicolon
    }
  }
  // TODO: move semicolon printing to declarations
  PPRINT(';'); // declarations end with an ;
#ifdef DEBUG
      PPRINT(std::string("ExternalDeclaration END"));
#endif
  PPRINT('\n');
}

PRETTY_PRINT(IdentifierDirectDeclarator) {
  /*TODO: unfinished*/
  PPRINT(this->identifier);
  if (help) {
    PPRINT(help);
  } 
}

Parameter::Parameter(TypeNode type, SubDeclarator declarator, Pos pos)
  : AstNode(pos), type(type), declarator(declarator) {
  
}

Parameter::Parameter(TypeNode type, Pos pos)
  : AstNode(pos), type(type) {
  
}

PRETTY_PRINT(Parameter) {
  /*TODO: unfinished*/
  PPRINT(type);
  if (declarator) {
    PPRINT(' ');
    PPRINT(declarator);
  }
}


PRETTY_PRINT(DeclaratorDirectDeclarator)
{
  if (help) {
    PPRINT('(');
  }
  PPRINT(declarator);
  if (help) {
    PPRINT(help);
    PPRINT(')');
  }
}



DirectDeclaratorHelp::DirectDeclaratorHelp(Pos pos) 
  : AstNode(pos)
{
  helperType = EPSILON;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(
    SubDirectDeclaratorHelp help,
    Pos pos
    ) : AstNode(pos), help(help)
{
  helperType = EMPTYLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(std::vector<ParameterNode> paramList,
    Pos pos)
  : AstNode(pos), paramList(paramList) 
{  
  helperType = PARAMETERLIST; 
}

DirectDeclaratorHelp::DirectDeclaratorHelp(std::vector<ParameterNode> paramList,
                                           SubDirectDeclaratorHelp help, Pos pos)
  : AstNode(pos), help(help), paramList(paramList) 
{
  helperType = PARAMETERLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(SubIdentifierList idList, Pos pos)
  : AstNode(pos), idList(idList) 
{
  helperType = IDENTIFIERLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(SubIdentifierList idList,
                                           SubDirectDeclaratorHelp help,
                                           Pos pos)
  : AstNode(pos), help(help), idList(idList)
{
  helperType = IDENTIFIERLIST;
}


PRETTY_PRINT(DirectDeclaratorHelp)
{
  switch (helperType) {
    case EPSILON:
      /*TODO: this is probably only a very fragile hack:
       * The reasoning for the following code is that if there's 'nothing' after a
       * identifier-direct-declarator, we must have read a parameterless function
       * TODO: check if this is always the case
       * */
#ifdef DEBUG
      PPRINT(std::string("===============fragile hack!=====================\n"));
#endif
      PPRINT('(');
      PPRINT(')');
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

SizeOfExpression::SizeOfExpression(std::pair<TypeNode, SubDeclarator> operand, Pos pos)
  : Expression(pos), operand(operand) 
{
}

PRETTY_PRINT(SizeOfExpression)
{
  PPRINT('(');
  PPRINT(operand.first);
  if (operand.second) {
    PPRINT(operand.second);
  }
  PPRINT(')');
}

ReturnStatement::ReturnStatement(Pos pos) : JumpStatement(pos) {}
ReturnStatement::ReturnStatement(SubExpression ex, Pos pos) 
  : JumpStatement(pos), expression(ex) {}


ContinueStatement::ContinueStatement(Pos pos) : JumpStatement(pos) {}

BreakStatement::BreakStatement(Pos pos) : JumpStatement(pos) {}

GotoStatement::GotoStatement(std::string label, Pos pos) 
  : JumpStatement(pos), label(label) {}

IterationStatement::IterationStatement(SubExpression ex,
    SubStatement st,
    IterationEnum k,
    Pos pos)
  : Statement(pos), expression(ex), statement(st), kind(k) {}


LabeledStatement::LabeledStatement(std::string str, SubStatement st, Pos pos)
  : Statement(pos), name(str), statement(st) {}


ExpressionStatement::ExpressionStatement(Pos pos) : Statement(pos) {}
ExpressionStatement::ExpressionStatement(SubExpression ex, Pos pos) 
  : Statement(pos), expression(ex) {}



DeclaratorDirectDeclarator::DeclaratorDirectDeclarator(SubDeclarator d,
        SubDirectDeclaratorHelp h,
        Pos pos) 
  : DirectDeclarator(pos), declarator(d), help(h) {}


DeclaratorDirectDeclarator::DeclaratorDirectDeclarator(SubDeclarator d,
        Pos pos) 
  : DirectDeclarator(pos), declarator(d) {}


IdentifierDirectDeclarator::IdentifierDirectDeclarator(std::string str,
    SubDirectDeclaratorHelp h,
    Pos pos) : DirectDeclarator(pos), identifier(str), help(h) {}

IdentifierDirectDeclarator::IdentifierDirectDeclarator(std::string str,
    Pos pos) : DirectDeclarator(pos), identifier(str) {}

IdentifierList::IdentifierList(std::vector<std::string > list, Pos pos)
  : AstNode(pos), nameList(list) {}


Declarator::Declarator(int cnt, SubDirectDeclarator ast, Pos pos)
  : AstNode(pos), pointerCounter(cnt), directDeclarator(ast) {}

#undef PPRINT
#undef ADDINDENT
#undef REMOVEINDENT
#undef PRETTY_PRINT
