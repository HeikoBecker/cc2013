#include <sstream>
#include <memory>
#include "ast.h"
#include "pprinter.h"
#include "parser.h"
#include "../utils/debug.h"
#include "../utils/exception.h"
#include "../lexer/punctuatortype.h"

using namespace std;
using namespace Parsing;
using namespace Semantic;

BinaryExpression::BinaryExpression(SubExpression lhs,
                                   SubExpression rhs,
                                   PunctuatorType op,
                                   Pos pos, const Pos* const operator_position) :
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
        if (!hasIntegerType(rhs)) {
        throw ParsingException(std::string(
              "Right operand of array subscript must have integer type, but is a !"
              + (rhs->getType() ?  rhs->getType()->toString() : "INITIALIZE ME!")), rhs->pos());
        }
        this->type = ltype->pointee();
        this->m_can_be_lvalue = true;
      } else if (auto rtype = std::dynamic_pointer_cast<PointerDeclaration>(rhs->getType())) {
        if (!hasIntegerType(lhs)) {
        throw ParsingException(std::string(
              "Left operand of array subscript must have integer type, but is a !"
              + (lhs->getType() ?  lhs->getType()->toString() : "INITIALIZE ME!")), lhs->pos());
        }
        this->type = rtype->pointee();
        this->m_can_be_lvalue = true;
      }  else {
        throw ParsingException(std::string(
              "Neither left nor right operand is a pointer! Left type is "
              + (lhs->getType() ?  lhs->getType()->toString() : "INITIALIZE ME!")), pos);
      }
      break;
    case PunctuatorType::ARROW: 
      // mostly the same as MEMBER_ACCESS, therefore we just change one variable
      // and (ab)use fall through
      if (auto ltype = std::dynamic_pointer_cast<PointerDeclaration>(lhs->getType())) {
        pointedToType = ltype->pointee();
        // A postfix expression followed by the -> operator and an identifier designates a member
        // of a structure or union object. The value is that of the named member of the object to
        // which the first expression points, and is an lvalue.
        this->m_can_be_lvalue = true;
      } else {
        throw ParsingException(std::string(  "Can't use operator -> on ")
                                           + (lhs->getType()? lhs->getType()->toString() : "INITIALIZE ME!")
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
          if (!this->m_can_be_lvalue) {
            // if we actually handle ARROW (instead of MEMBER_ACCESS) we must
            // not change the value of m_can_be_lvalue; luckily the parent
            // constructor always sets the value to false, and ARROW always sets
            // it to true before we reach this point; so if it is false, we know
            // that we are in the MEMBER_ACCESS case and have to react
            // accordingly:
            // 6.5.2.3 $3: and is an lvalue if the first expression is an lvalue
            this->m_can_be_lvalue = lhs->can_be_lvalue();
          }
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
        this->type = t;
        break;
      }
      throw ParsingException(std::string("Incompatible types for +"), lhs->pos());
    }
    case PunctuatorType::MINUS: {
      auto lhs_type = lhs->getType();
      auto rhs_type = rhs->getType();
      // For subtraction, one of the following shall hold:
      if (isArithmeticType(lhs_type) && isArithmeticType(rhs_type)) {
        // 1) both operands have arithmetic type
        this->type = applyUsualConversions(lhs_type, rhs_type).first;
        break;
      }
      auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs_type);
      if (isIntegerType(rhs_type)) {
        if (lhs_as_ptr) {
          if (!isCompleteObjectType(lhs_as_ptr->pointee())) {
            throw ParsingException(
                std::string("- requires pointer to point to complete object type"),
                lhs->pos()
            );
          }
          // 2) the left operand is a pointer to a complete object type and the
          // right operand has integer type.
          this->type = lhs_as_ptr;
          break;
        }
      }
      auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs_type);
      if (lhs_as_ptr && rhs_as_ptr) {
        if (!isCompleteObjectType(lhs_as_ptr->pointee())) {
          throw ParsingException(std::string("- requires pointer to point to complete object type"), lhs->pos());
        }
        // in real C this would be ptrdiff_t
        if (compareTypes(lhs_as_ptr->pointee(), rhs_as_ptr->pointee())) {
          // 3) both operands are pointers to qualified or unqualified versions
          // of compatible complete object types
          this->type = make_shared<IntDeclaration>();
        } else {
          throw ParsingException(lhs_type->toString()
              + " and " + rhs_type->toString()
              + " are not pointers to compatible types", pos);
        }
        break;
      }
      throw ParsingException(std::string("Incompatible types for -"), lhs->pos());
    }
    case PunctuatorType::LESS:
      if (hasRealType(lhs) && hasRealType(rhs)) {
        // TODO: apply usual conversions
        auto converted = applyUsualConversions(lhs->getType(),rhs->getType());
        this->type = make_shared<IntDeclaration>();
      } else {
        auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs->getType());
        auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs->getType());
        // TODO: function pointer conversion must fail
        if (lhs_as_ptr && rhs_as_ptr) {
          if (!(isObjectType(lhs_as_ptr->pointee()) && isObjectType(rhs_as_ptr->pointee()))) {
            throw ParsingException(std::string(
                  "Pointers must both point to object types, but lhs was "
                  + lhs_as_ptr->toString() + " and rhs was "
                  + rhs_as_ptr->toString()
                   ), this->pos());
          }
          if (compareTypes(lhs_as_ptr->pointee(), rhs_as_ptr->pointee())) {
            this->type = make_shared<IntDeclaration>();
          } else {
            throw ParsingException(
                std::string("Pointer point to different types: ")
                + lhs_as_ptr->pointee()->toString() + " and "
                + rhs_as_ptr->pointee()->toString(),
                this->pos());
          }
        } else {
          throw ParsingException(std::string(
                "Comparision requires both operands to be either pointer to object or to be of real type, but they were of type")
              + lhs->getType()->toString() + " and "
              + rhs->getType()->toString()
              , this->pos());
        }
      }
      break;
    case PunctuatorType::EQUAL:
    case PunctuatorType::NEQUAL:
      this->type = make_shared<IntDeclaration>();
      if (hasArithmeticType(lhs) && hasArithmeticType(rhs)) {
        // TODO: apply usual conversions
        this->type = make_shared<IntDeclaration>();
      } else {
        auto lhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(lhs->getType());
        auto rhs_as_ptr = dynamic_pointer_cast<PointerDeclaration>(rhs->getType());
        /* either both has to be pointers, or one has to be a pointer and the
         * othe one a null pointer constant (6.5.9 2)
         */
        if (lhs_as_ptr && rhs_as_ptr) {
          auto lpointee = lhs_as_ptr->pointee();
          auto rpointee = rhs_as_ptr->pointee();
          // one operand is a pointer to an object type and the other is a 
          // pointer to a qualified or unqualified version of void
          if (!(  (isObjectType(lpointee) && rpointee->type() == Semantic::Type::VOID)
              || (isObjectType(rpointee) && lpointee->type() == Semantic::Type::VOID))) {
            // both operands are pointers to qualified or unqualified versions 
            // of compatible types
            if (!compareTypes(lpointee, rpointee)) {
              ostringstream errmsg;
              errmsg << "Operands of equality operator are not compatible! Types were: "
                << lhs->getType()->toString() << " and "
                << rhs->getType()->toString();
              throw ParsingException(errmsg.str(), *operator_position);
            }
          }
        } else
        if (   (lhs_as_ptr && isNullPtrConstant(rhs))
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
      if (!hasScalarType(rhs)) {
        throw ParsingException(std::string("Logical operator requires operands with scalar type, but right operand is ") + (rhs->getType() ? rhs->getType()->toString() : "INITIALIZE ME!"), lhs->pos());
      }
      this->type = make_shared<IntDeclaration>();
      break;
    case PunctuatorType::ASSIGN:
      {
      /* 6.5.16 $2:
       * An assignment operator shall have a modifiable lvalue as its left operand.
       * WARNING: We currently don't have non-modifiable lvalues, because string
       * literals are not lvalues in our implementation and const does not exist;
       * as soon as this changes, the check below is not sufficiant enough
       */
      if (!lhs->can_be_lvalue()) {
        throw ParsingException("The left operand of an assignment must be a lvalue",
             *operator_position);
      }
      /* FIXME: this is probably wrong, allowing too much (can we really apply
       * the usual conversions here?)!
       * Check the legality of the following
       *  lhs is pointer, rhs is int => not allowed
       *  lhs is pointer, rhs is null pointer constant => done
       *  lhs is int, rhs is pointer => not allowed
       *  more?
       */
      auto valid = false;
      if (compareTypes(lhs->getType(), rhs->getType())) {
        valid = true;
      } else if ((hasArithmeticType(lhs) && hasArithmeticType(rhs))) {
        auto types_after_conversion = applyUsualConversions(lhs->getType(), rhs->getType());
        if (compareTypes(types_after_conversion.first, types_after_conversion.second)) {
          valid = true;
        }
      }
      if (lhs->getType()->type() == Semantic::Type::POINTER) {
        if (isNullPtrConstant(rhs)) {
          valid = true;
        } else if (auto rhs_as_ptr = std::dynamic_pointer_cast<PointerDeclaration>(rhs->getType())) {
          if (rhs_as_ptr->pointee()->type() == Semantic::Type::VOID) {
            valid = true;
          }
        } else if (rhs->getType()->type() == Semantic::Type::FUNCTION) {
          auto rhs_type2fptr = make_shared<PointerDeclaration>(0,
              std::static_pointer_cast<FunctionDeclaration>(rhs->getType()));
          valid = compareTypes(lhs->getType(), rhs_type2fptr);
        }
      }
      if (isNullPtrConstant(rhs)) {
        auto lhs_type = lhs->getType();
        switch (lhs_type->type()) {
          case Semantic::Type::INT:
          case Semantic::Type::CHAR:
            valid = true;
            break;
          default:
            break;
        }
      }
      if (auto lhs_as_ptr = std::dynamic_pointer_cast<PointerDeclaration>(lhs->getType())) {
        if (lhs_as_ptr->pointee()->type() == Semantic::Type::VOID) {
          if (rhs->getType()->type() == Semantic::Type::POINTER) {
            valid = true;
          }
        }
      }
      this->type = lhs->getType();
      if (!valid) {
        std::stringstream errmsg;
        errmsg << "Assignment invalid, <descriptive error messages here>!\n"
               << "lhs has type " << lhs->getType()->toString() << '\n'
               << "rhs has type " << rhs->getType()->toString() << '\n';
        throw ParsingException(errmsg.str(), pos);
      }
      break;
      }
    default:
      throw ParsingException(std::string() + "Implement this! " + Lexing::PunctuatorType2String(op), pos);
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
        // dereferencing a pointer yields the type of the pointee
        this->type = optype->pointee();
        if (isCompleteObjectType(this->type)) {
          // 6.5.3.2 $4
          // If the operand points to a function, the result is a function
          // designator; if it points to an object, the result is an lvalue
          // designating the object.
          this->m_can_be_lvalue = true;
        } else if (isIncompleteType(optype->pointee())) {
          throw ParsingException("Cannot dereference pointer to incomplete type "
                                 + optype->pointee()->toString(), pos);
        }
      } else if (auto optype = dynamic_pointer_cast<FunctionDeclaration>(operand->getType())) {
        // function  is convertible to pointer to function
        // when dereferenced, we get the function again 
        this->type = optype;
      } else {
        throw ParsingException(std::string("Cannot dereference ") 
                               + (operand->getType() ? operand->getType()->toString() : "INITIALIZE ME!"),
                               operand->pos());
      }
      break;
    case PunctuatorType::AMPERSAND:
      {
      // TODO: check that operand is lvalue, function designator, or operand of
      // [] or *
      // Multiple possibilities:
      // 1) lvalue that designates an object that is not a bit-field and is
      // not declared with the register storage-class specifier
      auto valid = false;
      if (operand->can_be_lvalue() && hasObjectType(operand)) {
        valid = true;
      } else if (operand->getType()->type() == Semantic::Type::FUNCTION) {
        //2) a function designator
        valid = true;
      } else if (auto operand_as_unary = std::dynamic_pointer_cast<UnaryExpression>(operand) ) {
        //3) the result of a [] or unary * operator
        if (   operand_as_unary->op == PunctuatorType::ARRAY_ACCESS
            || operand_as_unary->op == PunctuatorType::STAR) {
          valid = true;
        }
      }
      if (valid) {
        this->type = make_shared<PointerDeclaration>(0, operand->getType());
      } else {
        throw ParsingException("Incompatible opreand for &", operand->pos());
      }
      break;
      }
    case PunctuatorType::MINUS:
      if (!hasArithmeticType(operand)) {
        ostringstream errmsg;
        errmsg << "Operator - requires an arithmetic type, but got "
               <<  operand->getType()->toString()
               << "\n";
        throw ParsingException(errmsg.str(), operand->pos());
      } else {
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
      throw ParsingException(std::string() + "Implement this! " + Lexing::PunctuatorType2String(op), pos);
      break;
  }
}

VariableUsage::VariableUsage(std::string name, Pos pos, 
                             SemanticTreeNode semanticTree) 
  : Expression(pos), name(name), semanticTree(semanticTree) {
    this->m_can_be_lvalue = true;
  }

SemanticDeclarationNode VariableUsage::getType() {
  if (!this->type) {
    this->type = semanticTree->lookUpType(name, pos());
  }
  return this->type;

}

void VariableUsage::checkSemanticConstraints() {
  // check if the variable is declared
  this->getType();
}

SemanticDeclarationNode VariableUsage::getType(SubSemanticNode s) {
  if (!this->type) {
    try {
      this->type = s->getNode(this->name);
    } catch (const SemanticException & e) {
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
  this->type = make_shared<ArrayDeclaration>(
      make_shared<CharDeclaration>(),  // type
      name.size() + 1 // one more than the size to store '\0'
      );
  /* TODO: For reasons descibed in 
   * http://stackoverflow.com/questions/10004511/why-are-string-literals-l-value-while-all-other-literals-are-r-value
   * string literals are lvalues (see 6.5.1 $4). So in theory we need to set
   * this->m_can_be_lvalue = true
   * But I currently don't see any reason to do so
   */
}

Constant::Constant(std::string name, Pos pos, Lexing::ConstantType ct)
  : Expression(pos), ct(ct), name(name) 
{
  switch (ct) {
    case Lexing::ConstantType::CHAR:
      // 6.4.4.4 $10
      // An integer character constant has type int. 
      this->type = make_shared<IntDeclaration>();
      break;
    case Lexing::ConstantType::NULLPOINTER:
      // One can't decide which type a nullpointer has without knowing in which
      // context it is used
      this->type = make_shared<NullDeclaration>();
      break;
    case Lexing::ConstantType::INT:
    default:
      this->type = make_shared<IntDeclaration>();
      break;
  }
}


FunctionCall::FunctionCall(SubExpression funcName,
                           std::vector<SubExpression> arguments, Pos pos)
        : Expression(pos), funcName(funcName), arguments(arguments) 
{
  auto type = funcName->getType();
  if (type->type() == Semantic::Type::POINTER) {
    type = std::static_pointer_cast<PointerDeclaration>(type)->pointee();
  }
  if (type->type() == Semantic::Type::FUNCTION) {
    auto function = std::static_pointer_cast<FunctionDeclaration>(type);
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
  auto lhs_type = lhs->getType();
  auto rhs_type = rhs->getType();
  auto types_are_equal = Semantic::compareTypes(lhs_type, rhs_type);
  auto valid = false;
  // One of the following shall hold true for the second and third operand
  // both operands have arithmetic type;
  if (hasArithmeticType(lhs) && hasArithmeticType(rhs)) {
    valid = true;
    // If both the second and third operands have arithmetic type, the result type that would be
    // determined by the usual arithmetic conversions, were they applied to those two operands,
    // is the type of the result.
    this->type = applyUsualConversions(lhs_type, rhs_type).first;
  }
  //both operands have the same structure or union type  // we don't have union
  if (!valid) {
    if (   lhs_type->type() == Semantic::Type::STRUCT
             && types_are_equal) {
      valid = true;
      //If both the operands have structure or union type, the result has that type
      this->type = lhs->getType();
    }
  }
  // both operands have void type
  if (!valid) {
    // TODO save value of compareTypes for reuse
    valid = (   lhs_type->type() == Semantic::Type::VOID
             && types_are_equal);
    //If both operands have void type, the result has void type.
    this->type = lhs_type;
  }
  // both operands are pointers to qualified or unqualified versions of
  // compatible types
  auto lhs_is_a_pointer = lhs_type->type() == Semantic::Type::POINTER;
  auto rhs_is_a_pointer = rhs_type->type() == Semantic::Type::POINTER;
  if (!valid) {
    if (  lhs_is_a_pointer && rhs_is_a_pointer) {
      auto lhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(lhs_type);
      auto rhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(rhs_type);
      valid = compareTypes(lhs_as_ptr, rhs_as_ptr);
      if (valid) {
        this->type = lhs_as_ptr;
      }
    }
  }
  // one operand is a pointer and the other is a null pointer constant
  if (!valid) {
    if (lhs_is_a_pointer && isNullPtrConstant(rhs)) {
      valid = true;
      this->type = lhs_type;
    } else if (rhs_is_a_pointer && isNullPtrConstant(lhs)) {
      valid = true;
      this->type = rhs_type;
    }
  }
  // one operand is a pointer to an object type and the other is a pointer to a 
  // qualified or unqualified version of void.
  if (!valid) {
    if (lhs_is_a_pointer && rhs_is_a_pointer) {
      auto lhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(lhs_type);
      auto rhs_as_ptr = std::static_pointer_cast<PointerDeclaration>(rhs_type);
      auto lhs_pointee = lhs_as_ptr->pointee();
      auto rhs_pointee = rhs_as_ptr->pointee();
      // otherwise, one operand is a pointer to void or a qualified version of 
      // void, in which case the result type is a pointer to an appropriately 
      // qualified version of void.
      if (   isObjectType(lhs_pointee) && rhs_pointee->type() == Semantic::Type::VOID) {
        valid = true;
        this->type = rhs_type;
      }
      if (isObjectType(rhs_pointee) && lhs_pointee->type() == Semantic::Type::VOID) {
        valid = true;
        this->type = lhs_type;
      }
    }
  }

  if (!valid) {
    errmsg << "Second and third operand of conditional operator have unsuiting types "
           << lhs_type->toString() << " and " << rhs_type->toString();
    throw ParsingException(errmsg.str(), pos);
  }
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
  ex->checkSemanticConstraints();
  if (!hasScalarType(ex)) {
    throw ParsingException(
        "Controlling expression of selection statement must have scalar type",
        pos
        );
  };
  ifStatement = ifStat;
}

// use delegating constructor
SelectionStatement::SelectionStatement(
  SubExpression ex, 
  SubStatement ifStat, 
  SubStatement elseStat,
  Pos pos) : SelectionStatement(ex, ifStat, pos) 
{
  elseStatement = elseStat;
}


Declaration::Declaration(TypeNode t, SubDeclarator declarator, Pos pos, shared_ptr<SemanticTree> semanticTree)
  : AstNode(pos), type(t),declarator(declarator), semanticTree(semanticTree) {
    declNode = semanticTree->addDeclaration(type, declarator, pos);
}


Declaration::Declaration(TypeNode t, Pos pos)
  : AstNode(pos), type(t){}


FunctionDefinition::FunctionDefinition(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree
                        )
  : ExternalDeclaration(type, declarator, pos, semanticTree, false),
    compoundStatement(compoundStatement)
{
}

ExternalDeclaration::ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        Pos pos,
                        shared_ptr<SemanticTree> semanticTree,
                        bool assign
                        )
  : AstNode(pos), type(type), declarator(declarator),
    semanticTree(semanticTree)
{
  if (assign) {
    if (semanticTree) {
      declNode = semanticTree->addDeclaration(type, declarator, pos);
      if (!Semantic::isValidType(declNode)) {
        throw ParsingException("Illegal type: " + declNode->toString(), pos);
      }
    }
  }else {
    auto name = declarator->getIdentifier();
    declNode = semanticTree-> lookUpType(name, pos);
  }

//  std::cout<<"NAME: "<<declarator->getIdentifier()<<std::endl;
//  std::cout<<"Node: "<<declNode->toString()<<std::endl;

  // add the type
  
}

ExternalDeclaration::ExternalDeclaration(TypeNode type, Pos pos
, shared_ptr<SemanticTree> semanticTree
)
  : AstNode(pos), type(type), semanticTree(semanticTree)
{
  // std::cout<<"NAME: "<<declarator->getIdentifier()<<std::endl;
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
  // TODO: share code with assignment (operator =)
  // Get the type of the function which in which we are
  auto function_type = SemanticForest::filename2SemanticTree(this->pos().name)->currentFunction();
  // extract the return type from it
  auto expected_type = std::dynamic_pointer_cast<FunctionDeclaration>(function_type)->returnType();
  // check if expected is pointer and actual is null pointer constant
  // TODO: this should probably move into its own function
  if (  expected_type->type() == Semantic::Type::POINTER 
      && dynamic_pointer_cast<NullDeclaration>(actual_type)) {
    return; // types are compatible
  }

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
  : Statement(pos), expression(ex), statement(st), kind(k) 
{
  if (!hasScalarType(expression))   {
    throw ParsingException(
        "Controlling expression of an iteration statement must have scalar type",
        pos
        );
  }
}


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
       
        if (!paramList[0]->hasName()) {
          throw ParsingException("parameter name omitted ", paramList[0]->pos());
        }

        return paramList[0]->hasName();
      } else {
        // all declarators have to have a name
        for (int n=0; n<len; n++) {
          if(!paramList[n]->hasName()) {
            throw ParsingException("parameter name omitted ", paramList[n]->pos());
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
