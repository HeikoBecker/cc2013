#include <memory>
#include "ast.h"
#include "pprinter.h"
#include "parser.h"
#include "parserException.h"
#include "../utils/debug.h"

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
  if (op == PunctuatorType::ARRAY_ACCESS) {
    // left operand must have type pointer to object _type_
    if (auto ltype = std::dynamic_pointer_cast<PointerDeclaration>(lhs->getType())) {
      // right operand must have integer type
    } else {
      throw ParsingException(std::string(
              "Left operand does not point to an object, but is a !"
            + (lhs->getType() ?  lhs->getType()->toString() : "INITIALIZE ME!")), lhs->pos());
    }
  }
}

UnaryExpression::UnaryExpression(PunctuatorType op, SubExpression operand, Pos pos) :
  Expression(pos), operand(operand), op(op)
{
}

VariableUsage::VariableUsage(std::string name, Pos pos, 
                             SemanticTreeNode semanticTree) 
  : Expression(pos), name(name), semanticTree(semanticTree) {
    

  //pair<TypeNode, int> p = semanticTree->lookUpType(name, pos);
}

Literal::Literal(std::string name, Pos pos)
  : Expression(pos), name(name) {}

Constant::Constant(std::string name, Pos pos)
  : Expression(pos), name(name) {}


FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments, Pos pos)
        : Expression(pos), funcName(funcName), arguments(arguments) {;}

TernaryExpression::TernaryExpression(SubExpression condition, 
                                     SubExpression lhs, 
                                     SubExpression rhs,
                                     Pos pos)
     : Expression(pos), condition(condition), lhs(lhs), rhs(rhs)
{
 //TODO: type checking
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

StructType::StructType(std::string name, Pos pos) : Type(pos), name(name) {
  // default string is empty
  mycontent = std::vector<std::pair<TypeNode, SubDeclarator>> ();
  content = StructContent();
  hasDeclaration = false;
}

StructType::StructType(Pos pos) : Type(pos) {
  // default string is empty
  name = std::string("");
  content = StructContent();
  mycontent = std::vector<std::pair<TypeNode, SubDeclarator>> ();
  hasDeclaration = false;
}

StructType::StructType(std::string name, StructContent strcontent, Pos pos)
  : Type(pos), name(name), content(strcontent) {
  mycontent = std::vector<std::pair<TypeNode, SubDeclarator>> ();
  hasDeclaration = true;

  for(auto cont : strcontent) {
    mycontent.push_back(make_pair(cont.first,cont.second[0].first));
  }
}

CompoundStatement::CompoundStatement(std::vector<BlockItem> subStatements, Pos pos)
  : Statement(pos), subStatements(std::move(subStatements))
{

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


Declaration::Declaration(TypeNode t, SubDeclarator declarator, Pos pos, shared_ptr<SemanticTree> semanticTree)
  : AstNode(pos), type(t),declarator(declarator), semanticTree(semanticTree) {
    semanticTree->addDeclaration(type, declarator, pos);
}


Declaration::Declaration(TypeNode t, Pos pos)
  : AstNode(pos), type(t){}


ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        )
  : AstNode(pos), type(type), declarator(declarator),
    compoundStatement(compoundStatement), semanticTree(semanticTree)
{
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
    ) : AstNode(pos), externalDeclarations(externalDeclarations) 
{
  if (externalDeclarations.empty()) {
    throw ParsingException("A translationUnit must not be empty!", pos);
  }
}

Parameter::Parameter(TypeNode type, SubDeclarator declarator, Pos pos)
  : AstNode(pos), type(type), declarator(declarator) {
  
}

Parameter::Parameter(TypeNode type, Pos pos)
  : AstNode(pos), type(type) {
  
}



//DirectDeclaratorHelp::DirectDeclaratorHelp(Pos pos) 
  //: AstNode(pos)
//{
  //helperType = EPSILON;
//}

DirectDeclaratorHelp::DirectDeclaratorHelp(
    Pos pos
    ) : AstNode(pos)
{
  helperType = EMPTYLIST;
}

DirectDeclaratorHelp::DirectDeclaratorHelp(std::vector<ParameterNode> paramList,
    Pos pos)
  : AstNode(pos), paramList(paramList) 
{  
  helperType = PARAMETERLIST; 
}

DirectDeclaratorHelp::DirectDeclaratorHelp(SubIdentifierList idList, Pos pos)
  : AstNode(pos), idList(idList) 
{
  helperType = IDENTIFIERLIST;
}


SizeOfExpression::SizeOfExpression(std::pair<TypeNode, SubDeclarator> operand, Pos pos)
  : Expression(pos), operand(operand) 
{
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

bool Parameter::hasDeclarator() {
  return declarator ? true : false;
}

bool Parameter::hasName() {
  if (declarator) {
    return declarator->hasName();
  } 

  return false; 
}

bool Parameter::isVoid() {
 return getType()->isVoid() && !hasDeclarator();
}


bool DirectDeclaratorHelp::containsOnlyOneVoidIfSpecified() {
  if(idList) {
    // don't accept idList
    return false;
  } else {

    int voidCounter  =0;

    for (auto p : paramList) {
      if(p->isVoid()) {
        voidCounter++;
      }
    }

    if (voidCounter == 0) {
      return true;
    } else if (voidCounter == 1){
      return paramList.size() == 1;
    } else {
      return false;
    }
  }
}

bool DirectDeclaratorHelp::canBeFunction() {
    if(idList) {
      return false;
    } else {
      int len = paramList.size();
      // empty is ok
      if (len == 0) {
        return true;
      } else if (len == 1) {
        // void is ok
        if (paramList[0]->isVoid()) {
          return true;
        } 

        return paramList[0]->hasName();
      } else {
        // all declarators have to have a name
        for (int n=0; n<len; n++) {
          if(!paramList[n]->hasName()) {
            return false;
          }
        }
        return true;
      }
    }
    return true;
}

DeclaratorDirectDeclarator::DeclaratorDirectDeclarator(SubDeclarator d,
        std::vector<SubDirectDeclaratorHelp> h,
        Pos pos) 
  : DirectDeclarator(pos), declarator(d), help(h) {}

DeclaratorDirectDeclarator::DeclaratorDirectDeclarator(SubDeclarator d,
        Pos pos) 
  : DirectDeclarator(pos), declarator(d) {}


IdentifierDirectDeclarator::IdentifierDirectDeclarator(std::string str,
    std::vector<SubDirectDeclaratorHelp> h,
    Pos pos) : DirectDeclarator(pos), identifier(str), help(h) {}

IdentifierDirectDeclarator::IdentifierDirectDeclarator(std::string str,
    Pos pos) : DirectDeclarator(pos), identifier(str) {}

IdentifierList::IdentifierList(std::vector<std::string > list, Pos pos)
  : AstNode(pos), nameList(list) {}


Declarator::Declarator(int cnt, SubDirectDeclarator ast, Pos pos)
  : AstNode(pos), pointerCounter(cnt), directDeclarator(ast) {}
