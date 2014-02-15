#include <sstream>
#include <memory>
#include "ast.h"
#include "pprinter.h"
#include "parser.h"
#include "parserException.h"
#include "../utils/debug.h"

using namespace Parsing;
using namespace Semantic;

BinaryExpression::BinaryExpression(SubExpression lhs,
                                   SubExpression rhs,
                                   PunctuatorType op,
                                   Pos pos) :
  Expression(pos),
  lhs(lhs),
  rhs(rhs),
  op(op)
{
  SemanticDeclarationNode pointedToType;
  switch (op) {
    case PunctuatorType::ARRAY_ACCESS:
      // 6.5.2.1
      // left operand must have type pointer to object _type_
      if (auto ltype = std::dynamic_pointer_cast<PointerDeclaration>(lhs->getType())) {
        // right operand must have integer type
      } else {
        throw ParsingException(std::string(
              "Left operand does not point to an object, but is a !"
              + (lhs->getType() ?  lhs->getType()->toString() : "INITIALIZE ME!")), lhs->pos());
      }
      break;
    case PunctuatorType::ARROW: 
      // mostly the same as MEMBER_ACCESS, therefore we just change one variable
      // and (ab)use fall through
      if (auto ltype = std::dynamic_pointer_cast<PointerDeclaration>(lhs->getType())) {
        pointedToType = ltype->pointee();
      } else {
        throw ParsingException(std::string(  "Can't use operator -> on ")
                                           + (ltype ? ltype->toString() : "INITIALIZE ME!")
                                           + ", pointer type required",
                               lhs->pos());
      }
    case PunctuatorType::MEMBER_ACCESS:
      // 6.5.2.3
      // first operator shall have an atomic, qualified, or unqualified structure or union type
      if (auto ltype = std::dynamic_pointer_cast<StructDeclaration>( (op == PunctuatorType::MEMBER_ACCESS) ?  lhs->getType() : pointedToType )) {
        // identifier must follow
        if (auto identifier = std::dynamic_pointer_cast<VariableUsage>(rhs)) {
          this->type = identifier->getType(ltype->node());
        } else {
          throw ParsingException(std::string(
              "Trying to access struct member, but right operand is not an identifier , but a "
              + (rhs->getType() ?  rhs->getType()->toString() : "INITIALIZE ME!")), lhs->pos());
        }
      } else {
        throw ParsingException(std::string(
              "Trying to access struct member, but left operand is not a struct, but a "
              + (lhs->getType() ?  lhs->getType()->toString() : "INITIALIZE ME!")), lhs->pos());
      }
      break;
    case PunctuatorType::STAR:
      // 6.5.5
      if (!hasArithmeticType(lhs)) { // TODO: should this be put into a function?
        throw new ParsingException(std::string(
              "Multiplication requires that the left operand has arithmetic type"),
            lhs->pos()
            );
      }
      if (!hasArithmeticType(rhs)) {
        throw new ParsingException(std::string(
              "Multiplication requires that the right operand has arithmetic type"),
            rhs->pos()
            );
      }
      this->type = make_shared<IntDeclaration>();
      break;
    case PunctuatorType::PLUS: {
      auto lhs_type = lhs->getType();
      auto rhs_type = rhs->getType();
      if (isArithmeticType(lhs_type) && isArithmeticType(rhs_type)) {
        // TODO: apply usual conversions
        this->type = make_shared<IntDeclaration>();
        break;
      }
      shared_ptr<PointerDeclaration> t;
      if (isIntegerType(lhs_type)) {
        t = dynamic_pointer_cast<PointerDeclaration>(rhs_type);
      } else if (isIntegerType(rhs_type)) {
        t = dynamic_pointer_cast<PointerDeclaration>(lhs_type);
      }
      if (t) {
        this->type = t->pointee();
        break;
      }
      throw ParsingException(std::string("Incompatible types for +"), lhs->pos());
    }
    case PunctuatorType::MINUS: {
      auto lhs_type = lhs->getType();
      auto rhs_type = rhs->getType();
      if (isArithmeticType(lhs_type) && isArithmeticType(rhs_type)) {
        // TODO: apply usual conversions
        this->type = make_shared<IntDeclaration>();
        break;
      }
      auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs_type);
      if (isIntegerType(rhs_type)) {
        if (rhs_as_ptr) {
          this->type = rhs_as_ptr->pointee();
          break;
        }
      }
      auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs_type);
      if (lhs_as_ptr && rhs_as_ptr) {
        // in real C this would be ptrdiff_t
        this->type = make_shared<IntDeclaration>();
      }
      throw ParsingException(std::string("Incompatible types for -"), lhs->pos());
    }
    case PunctuatorType::LESS:
      if (hasRealType(lhs) && hasRealType(rhs)) {
        // TODO: apply usual conversions
        this->type = make_shared<IntDeclaration>();
      } else {
        auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs->getType());
        auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs->getType());
        // TODO: function pointer conversion must fail
        if (lhs_as_ptr && rhs_as_ptr) {
          this->type = make_shared<IntDeclaration>();
        } else {
          throw ParsingException(std::string("Comparision requires both operands to be either pointer to object or to be of real type."), this->pos());
        }
      }
      break;
    case PunctuatorType::EQUAL:
    case PunctuatorType::NEQUAL:
      if (hasRealType(lhs) && hasRealType(rhs)) {
        // TODO: apply usual conversions
        this->type = make_shared<IntDeclaration>();
      } else {
        auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs->getType());
        auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs->getType());
        /* either both has to be pointers, or one has to be a pointer and the
         * othe one a null pointer constant (6.5.9 2)
         */
        if (   (lhs_as_ptr && rhs_as_ptr)
            || (lhs_as_ptr && isNullPtrConstant(rhs))
            || (rhs_as_ptr && isNullPtrConstant(lhs))) {
          this->type = make_shared<IntDeclaration>();
        } else {
          throw ParsingException(std::string("Comparision requires both operands to be either pointer to object or to be of arithmetic type."), this->pos());
        }
      }
      break;
    case PunctuatorType::LAND:
    case PunctuatorType::LOR:
      if (!hasScalarType(lhs)) {
        throw ParsingException(std::string("Logical operator requires operands with scalar type, but left operand is ") + (lhs->getType() ? lhs->getType()->toString() : "INITIALIZE ME!"), lhs->pos());
      }
      if (!hasScalarType(lhs)) {
        throw ParsingException(std::string("Logical operator requires operands with scalar type, but right operand is ") + (rhs->getType() ? rhs->getType()->toString() : "INITIALIZE ME!"), lhs->pos());
      }
      this->type = make_shared<IntDeclaration>();
      break;
    case PunctuatorType::ASSIGN:
      // TODO: check that lhs is lvalue
      /* TODO: this needs more checks! Check the legality of the following
       *  lhs is pointer, rhs is int
       *  lhs is pointer, rhs is null pointer constant
       *  lhs is int, rhs is pointer
       *  more?
       */
      if (!(hasArithmeticType(lhs) && hasArithmeticType(rhs))) {
        if (typeid(lhs->getType()).name() != typeid(rhs->getType()).name()) { // TODO: is operator== already implemented?
          throw ParsingException("More work remaining! ", pos);
        }
      }
      this->type = lhs->getType();
      break;
    default:
      throw ParsingException(std::string() + "Implement this! " + PunctuatorType2String(op), pos);
      break;
  }
}

UnaryExpression::UnaryExpression(PunctuatorType op, SubExpression operand, Pos pos) :
  Expression(pos), operand(operand), op(op)
{
  switch (op) {
    // 6.5.3.2 has some strange stuff in section 3 about & and *'s interplay
    case PunctuatorType::STAR:
      /* to understand this 
       * http://stackoverflow.com/questions/6893285/why-do-all-these-crazy-function-pointer-definitions-all-work-what-is-really-goi
       * is useful (though not a replacement for the standard) */
      if (auto optype = dynamic_pointer_cast<PointerDeclaration>(operand->getType()))  {
        this->type = make_shared<PointerDeclaration>(1, operand->getType());
      } else if (auto optype = dynamic_pointer_cast<FunctionDeclaration>(operand->getType())) {
        // function  is convertible to pointer to function 
        this->type = optype;
      } else {
        throw ParsingException(std::string("Cannot dereference ") 
                               + (operand->getType() ? operand->getType()->toString() : "INITIALIZE ME!"),
                               operand->pos());
      }
      break;
    case PunctuatorType::AMPERSAND:
      // TODO: check that operand is lvalue, function designator, or operand of
      // [] or *
      this->type = make_shared<PointerDeclaration>(1, operand->getType());
      break;
    case PunctuatorType::MINUS:
      if (!hasArithmeticType(operand)) {
        throw ParsingException(std::string("Operator - requires an arithmetic type"), operand->pos());
        this->type = promoteType(operand->getType());
      }
      break;
    case PunctuatorType::NOT:
      if (!hasScalarType(operand)) {
        throw ParsingException("Operator '!' requires an operand of scalar type", operand->pos());
      }
      this->type = make_shared<IntDeclaration>();
     break;
    case PunctuatorType::SIZEOF:
     if (dynamic_pointer_cast<FunctionDeclaration>(operand->getType())) {
        throw ParsingException("Illegal application of 'sizeof' to a function type", operand->pos());
     }
     // in real C, it would be size_t, but we don't have that one
     this->type = make_shared<IntDeclaration>();
     break;
    default:
      throw ParsingException(std::string() + "Implement this! " + PunctuatorType2String(op), pos);
      break;
  }
}

VariableUsage::VariableUsage(std::string name, Pos pos, 
                             SemanticTreeNode semanticTree) 
  : Expression(pos), name(name), semanticTree(semanticTree) {
    
}

SemanticDeclarationNode VariableUsage::getType() {
  if (!this->type) {
    this->type = semanticTree->lookUpType(name, pos());
  }
  return this->type;

}

SemanticDeclarationNode VariableUsage::getType(SubSemanticNode s) {
  if (!this->type) {
    try {
      this->type = s->getNode(this->name);
    } catch (SemanticException e) {
        throw ParsingException(std::string("Struct has no member " + name), this->pos());
    }
  }
  return this->type;

}

Literal::Literal(std::string name, Pos pos)
  : Expression(pos), name(name)
{
  /*WARNING: technically, the type is wrong
   * A stringliteral has actually the type char[]
   * however, we don't support arrays 
   * and when used it should decay to char* anyway
   */
  this->type = make_shared<PointerDeclaration>(1, make_shared<CharDeclaration>());
}

Constant::Constant(std::string name, Pos pos)
  : Expression(pos), name(name) 
{
  this->type = make_shared<IntDeclaration>();
}


FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments, Pos pos)
        : Expression(pos), funcName(funcName), arguments(arguments) 
{
  if (auto function = std::dynamic_pointer_cast<FunctionDeclaration>(funcName->getType())) {
    auto expected_parameter = function->parameter(); 
    if (expected_parameter.size() == arguments.size()) {
      // check if argument types match
      // if not, try to convert to desired type
      for (unsigned long i = 0; i < arguments.size(); ++i) {
        if (!Semantic::compareTypes(expected_parameter.at(i), arguments.at(i)->getType())) {
          // TODO: conversion is not correct atm
          auto promoted_expected = promoteType(expected_parameter.at(i));
          auto promoted_actually = promoteType(arguments.at(i)->getType());
          if (Semantic::compareTypes(promoted_actually, promoted_expected)) {
            continue;
          } else {
            std::cout << "promoted: expected "
                      << promoted_expected->toString()
                      << "got "
                      << promoted_expected->toString();
          }
          std::ostringstream errmsg;
          errmsg << "Expected argument of type "
                  << expected_parameter.at(i)->toString()
                  << " but got "
                  << (arguments.at(i)->getType() ? arguments.at(i)->getType()->toString()
                                                 : "INITIALIZE ME!");
          throw ParsingException(errmsg.str(), arguments.at(i)->pos());
        }
      }
      // all argument types match => no errors were found
      // the type of the function call is the return type of the function
      this->type = function->returnType();
    } else {
      std::ostringstream errmsg;
      errmsg  << function->toString() << " requires "
        << expected_parameter.size() << " parameters, but "
        << arguments.size() << " parameters were given.";
      throw ParsingException(errmsg.str(), pos);
    }
  } else {
    throw ParsingException(std::string("Trying to call ") 
        + (funcName->getType() ? funcName->getType()->toString() : "INITIALIZE ME!")
        + " which is not a function", pos);
  }
}

TernaryExpression::TernaryExpression(SubExpression condition, 
                                     SubExpression lhs, 
                                     SubExpression rhs,
                                     Pos pos)
     : Expression(pos), condition(condition), lhs(lhs), rhs(rhs)
{
  std::ostringstream errmsg;
  // 6.5.15: conditional operator
  //The first operand shall have scalar type.
  if (!hasScalarType(condition)) {
    errmsg << "The conditional operator requires that the first operand has scalar type, but it has type "
           << condition->getType()->toString();
    throw ParsingException(errmsg.str(), condition->pos());
  }
  // One of the following shall hold true for the second and third operand
  // both operands have arithmetic type;
  auto lhs_type = lhs->getType();
  auto rhs_type = rhs->getType();
  auto types_are_equal = Semantic::compareTypes(lhs_type, rhs_type);
  auto valid = (hasArithmeticType(lhs) && hasArithmeticType(rhs));
  //both operands have the same structure or union type  // we don't have union
  if (!valid) {
    valid = (   lhs_type->type() == Semantic::Type::STRUCT
             && types_are_equal);
  }
  // both operands have void type
  if (!valid) {
    // TODO save value of compareTypes for reuse
    valid = (   lhs_type->type() == Semantic::Type::VOID
             && types_are_equal);
  }
  // both operands are pointers to qualified or unqualified versions of
  // compatible types
  auto lhs_is_a_pointer = lhs_type->type() == Semantic::Type::POINTER;
  auto rhs_is_a_pointer = rhs_type->type() == Semantic::Type::POINTER;
  if (!valid) {
    // TODO: I coudn't find this in the standard, but it makes sense that we
    // apply the usual conversions to the pointee types before comparing
    if (  lhs_is_a_pointer && rhs_is_a_pointer) {
      auto lhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(lhs_type);
      auto rhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(rhs_type);
      auto promoted = applyUsualConversions(lhs_as_ptr->pointee(), rhs_as_ptr->pointee());
      valid = compareTypes(promoted.first, promoted.second);
      if (valid) {
        lhs_type = promoted.first;
        rhs_type = promoted.second;
      }
    }
  }
  // one operand is a pointer and the other is a null pointer constant
  if (!valid) {
    if (lhs_is_a_pointer && isNullPtrConstant(rhs)) {
      valid = true;
    } else if (rhs_is_a_pointer && isNullPtrConstant(lhs)) {
      valid = true;
    }
  }
  if (!valid) {
    errmsg << "Second and third operand of conditional operator have unsuiting types "
           << lhs_type->toString() << " and " << rhs_type->toString();
    throw ParsingException(errmsg.str(), pos);
  }
  this->type = lhs_type;
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
    if (!cont.second.empty()) {
      mycontent.push_back(make_pair(cont.first,cont.second[0].first));
    } else {
      // TODO: is this an issue? Or can this always happen?
      // The if check fixes a bug with tests/parser/pass/terminatedStruct.h
    }
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


FunctionDefinition::FunctionDefinition(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        )
  : ExternalDeclaration(type, declarator, pos, semanticTree),
    compoundStatement(compoundStatement)
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
  // TODO: do we need any further checks here? Or is the operand guarantueed to be
  // valid when the constructor is called
  this->type = make_shared<IntDeclaration>();
}

ReturnStatement::ReturnStatement(Pos pos) : JumpStatement(pos) 
{
  // single return without expression -> return type must be void
  auto actual_type = make_shared<VoidDeclaration>();
  verifyReturnType(actual_type);
}

ReturnStatement::ReturnStatement(SubExpression ex, Pos pos) 
  : JumpStatement(pos), expression(ex)
{
  // Get the type of the expression which we are returning
  auto actual_type = ex->getType();
  verifyReturnType(actual_type);
}

void ReturnStatement::verifyReturnType(SemanticDeclarationNode actual_type) {
  // Get the type of the function which in which we are
  auto function_type = SemanticForest::filename2SemanticTree(this->pos().name)->currentFunction();
  // extract the return type from it
  auto expected_type = std::dynamic_pointer_cast<FunctionDeclaration>(function_type)->returnType();
  // TODO: don't use this equality check, but check if the types are equal after
  // applying the "usual conversions"
  auto types_after_conversion = applyUsualConversions(actual_type, expected_type);
  if (!Semantic::compareTypes(types_after_conversion.first,
                             types_after_conversion.second)) {
    throw ParsingException(std::string("A ")
        + actual_type->toString()
        + " is returned, but a "
        + expected_type->toString()
        + " is expected!", pos());
  }
}


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
