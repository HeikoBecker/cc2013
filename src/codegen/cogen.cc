#include "cogen.h"
#include "ircreator.h"
#include "../parser/ast.h"
#include "../parser/astNode.h"
#include "../parser/statementNode.h"
#include "../parser/expressionNode.h"
#include "../parser/semantic.h"
#include "../utils/exception.h"
#include "../utils/util.h"
#include "labelgen.h"

#include <memory>
#include <algorithm>
#include <iterator>
#include <typeinfo> // only for debugging purpose

#define EMIT_IR(X) void X::emitIR(Codegeneration::IRCreator* creator)
#define EMIT_LV(X) llvm::Value* X::emit_lvalue(Codegeneration::IRCreator* creator) 
#define EMIT_RV(X) llvm::Value* X::emit_rvalue(Codegeneration::IRCreator* creator) 
#define EMIT_CONDITION(X) void X::emit_condition(\
    Codegeneration::IRCreator* creator,\
    llvm::BasicBlock* trueSuccessor,\
    llvm::BasicBlock* falseSuccessor\
    )


void Codegeneration::genLLVMIR(const char* filename, Parsing::AstRoot root) {

  std::string errorStr;
  llvm::raw_fd_ostream stream(filename, errorStr);
  Codegeneration::IRCreator Creator (filename);
  root->emitIR(&Creator);
  Creator.print(stream); /* M is a llvm::Module */
}

EMIT_IR(Parsing::AstNode)
{
  throw CompilerException(std::string(typeid(*this).name()) + " is not implemented yet!", this->pos());
  UNUSED(creator); //FIXME
}

EMIT_IR(Parsing::Declaration)
{
  llvm::Type *variable = creator->semantic_type2llvm_type(declNode);
  auto var = creator->allocateInCurrentFunction(variable);
  declNode->associatedValue = var;
  if (this->declarator->hasName()) {
    var->setName(this->declarator->getIdentifier());
  }
}

EMIT_IR(Parsing::TranslationUnit)
{
  // call emitIR on each external declaration of the translation unit
  for (auto external_declaration: this->externalDeclarations) {
    external_declaration->emitIR(creator);
  }
}

EMIT_IR(Parsing::ExternalDeclaration)
{
  //FIXME: use IRcreator
  // TODO: type mapping needs to go into a different method!
  using namespace llvm;
  // we either have to work with a global declaration or a forward declaration
  // function definitions are handled in FunctionDefinition
  // first we take the type of the node

  llvm::Type * external_declaration_type = creator->semantic_type2llvm_type(
      this->getSemanticNode()
  );
  auto name = this->declarator->getIdentifier();
  if (this->getSemanticNode()->type() == Semantic::Type::FUNCTION) {
    this->getSemanticNode()->associatedValue = creator->startFunction(
        static_cast<FunctionType*>(external_declaration_type),
        name,
        false);
    return;
  }
  
  // TODO: move global variable creato into creator method?
  GlobalVariable *GlobVar = creator->makeGlobVar(external_declaration_type);
   //TODO: what should we do with the global variable now?
  GlobVar->setName(name); // FIXME: we probably want a get name method
  this->getSemanticNode()->associatedValue = GlobVar;
}

EMIT_IR(Parsing::FunctionDefinition)
{
  // TODO: make name a member of functiondefiniton or declaration
  auto name = this->declarator->getIdentifier();
  // lookup the type of the current function
  auto semtree = Parsing::SemanticForest::filename2SemanticTree(this->pos().name);
  auto function_type_ = std::static_pointer_cast<FunctionDeclaration>(
      semtree->lookUpType(name, this->pos())
  );
  auto function_type = static_cast<llvm::FunctionType*>(creator->semantic_type2llvm_type(function_type_));
  auto function = creator->startFunction(function_type, name);
  auto parameter_index = 0;
  std::for_each(function->arg_begin(), function->arg_end(),
      [&](decltype(function->arg_begin()) argument){
      auto param = function_type_->parameter()[parameter_index];
      // 1. Allocate a stack slot
      auto ptr = creator->allocateInCurrentFunction(argument->getType());
      // 2. Store the argument value
      creator->store(argument, ptr);
      // 3. associate type with value
      param->associatedValue = ptr;
      ++parameter_index;
  });
  function_type_->associatedValue = function;
  
  // emit code for the body
  this->compoundStatement->emitIR(creator);
  creator->finishFunction();
  llvm::verifyFunction(*function);
}

EMIT_IR(Parsing::CompoundStatement)
{
  for (auto statement : this->subStatements) {
    statement->emitIR(creator);
  }
}

EMIT_IR(Parsing::ReturnStatement)
{
  if (this->expression) {
    auto value = expression->emit_rvalue(creator);
    if (!value) {
      throw ParsingException("rvalue is null, this is a bug", pos());
    }
    creator->makeReturn(value);
  } else {
    creator->makeReturn(nullptr);
  }
}

EMIT_IR(Parsing::LabeledStatement) {
  creator->makeBlock(name);
  statement->emitIR(creator);
}

EMIT_IR(Parsing::IterationStatement) {
  auto headerBlock = creator->makeBlock("while_header");

  auto contentBlock = creator->makeBlock("while_content", false);
  auto endBlock = creator->makeBlock("while_end", false);
  expression->emit_condition(creator, contentBlock, endBlock);

  creator->setCurrentBasicBlock(contentBlock);

  if (statement) { // TODO should always exist? 
    statement->emitIR(creator);
  }

  creator->connect(headerBlock);
  creator->setCurrentBasicBlock(endBlock);
}

EMIT_IR(Parsing::SelectionStatement)
{
  creator->makeBlock("if-header");

  auto consequenceBlock = creator->makeBlock("if-consequence", false);
  auto alternativeBlock = creator->makeBlock("if-alternative", false);
  auto endBlock = creator->makeBlock("if-end", false);
  this->expression->emit_condition(creator, consequenceBlock, alternativeBlock);
  creator->setCurrentBasicBlock(consequenceBlock);
  this->ifStatement->emitIR(creator);
  creator->connect(nullptr, endBlock);
  if (this->elseStatement) {
    creator->setCurrentBasicBlock(alternativeBlock);
    this->elseStatement->emitIR(creator);
    creator->connect(nullptr, endBlock);
  } else {
    creator->connect(alternativeBlock, endBlock);
  }
  creator->setCurrentBasicBlock(endBlock);
}

//##############################################################################
//#                    Expression Code Generation                              #
//##############################################################################

EMIT_CONDITION(Parsing::Expression) //FIXME: Why does an expression need EMIT_CONDITION???
{
  auto condition = this->emit_rvalue(creator);
  creator->makeConditonalBranch(condition, trueSuccessor, falseSuccessor);
}

EMIT_CONDITION(Parsing::BinaryExpression)
{
  switch (op) {
    case PunctuatorType::LAND: 
      {
      auto shortCircuitingBB = creator->getControlFlowBlock();
      lhs->emit_condition(creator, shortCircuitingBB, falseSuccessor);
      creator->setCurrentBasicBlock(shortCircuitingBB);
      rhs->emit_condition(creator, trueSuccessor, falseSuccessor);
      }
      break;
    case PunctuatorType::LOR:
      {
      auto shortCircuitingBB = creator->getControlFlowBlock();
      lhs->emit_condition(creator, trueSuccessor, shortCircuitingBB);
      creator->setCurrentBasicBlock(shortCircuitingBB);
      rhs->emit_condition(creator, trueSuccessor, falseSuccessor);
      }
      break;
    default:
      Parsing::Expression::emit_condition(creator, trueSuccessor, falseSuccessor);
  }
}

/*
 * An expression can be part of a statement with e; where e is a statement.
 * So we need! emit_rvalue to produce the rvalue.
 * The rvalue function will be overwritten by the corresponding class so we can
 * just call it here
 */

EMIT_IR(Parsing::ExpressionStatement)
{
  if (expression) {
    this->expression->emit_rvalue(creator);
  }
}

/*
 * emitIR should not be called directly! Every time we need a value of it, we 
 * need to know wether we want the lvalue or the rvalue. 
 * --> throw exception here
 */
EMIT_IR(Parsing::Expression)
{
        UNUSED(creator);
        throw ParsingException("Illegal Method call for emitIR on an expression",
                        this->pos());
}

/*
 * Methods for abstract expression object. They should never be called. If this
 * happens, we forgot to override the method at a specific place, that will be
 * printed by the exception.
 */
EMIT_RV(Parsing::Expression) {
  UNUSED(creator);
  throw CompilerException(std::string("You did not override the method emit_rvalue for")
        + typeid(*this).name(), this->pos());
}
EMIT_LV(Parsing::Expression) {
  UNUSED(creator);
  throw CompilerException(std::string("You did not override the method emit_lvalue for")
      + typeid(*this).name()
      , this->pos());
}

/*
 * Creates the RVALUE of the BinaryExpression object and returns it for further
 * usage. First compute left and right values, then emit the instruction based 
 * on the operand
 */
EMIT_RV(Parsing::BinaryExpression) {
  //First compute the values for the subexpressions
  // Not every operator requires lvalues! And the emit methods have side
  // effects, so they mustn't be called when the value is not required
  // We always need the rvalue of the right side. Precompute it and initialize
  // left side to nullptr
  llvm::Value* lhs = nullptr;
  llvm::Value* rhs = this->rhs->emit_rvalue(creator);

  //then compute the corresponding value based on the operand
  //the creator will do the llvm magic. We just want to find the
  //correct method to call
  switch(this->op){
	case PunctuatorType::PLUS:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createAdd(lhs, rhs);
	case PunctuatorType::MINUS:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createMinus(lhs, rhs);
	case PunctuatorType::LESS:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createLess(lhs, rhs);
	case PunctuatorType::STAR:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createMult(lhs, rhs);
	case PunctuatorType::NEQUAL:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createUnequal(lhs, rhs);
	case PunctuatorType::EQUAL:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createEqual(lhs, rhs);
	case PunctuatorType::LAND:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createLogAnd(lhs,rhs);
        case PunctuatorType::LOR:
          lhs = this->lhs->emit_rvalue(creator);
          return creator->createLogOr(lhs, rhs);
        case PunctuatorType::MEMBER_ACCESS:
	case PunctuatorType::ARROW: {
          lhs = this->lhs->emit_rvalue(creator);
          //We need to find the correct index inside the struct
          //First get the struct type
          int index = creator->computeIndex(this->lhs, this->rhs);
         if(this->op == PunctuatorType::ARROW)
            //create the acces with the  correct index
            return creator->createPointerAccess(lhs, rhs, index);
          else
            return creator->createAccess(lhs, rhs, index);
                                    }
	case PunctuatorType::ASSIGN:{
          lhs = this->lhs->emit_lvalue(creator);
          llvm::Type* type = creator->semantic_type2llvm_type(this->lhs->getType());
          return creator->createAssign(lhs,rhs, type);
                                    }
	default:
	  throw CompilerException("INTERNAL ERROR", this->pos());
	}
}

EMIT_LV(Parsing::BinaryExpression){
        llvm::Value* lhs = this->lhs->emit_lvalue(creator);
        llvm::Value* rhs = this->rhs->emit_lvalue(creator);

        switch (this->op){
        case PunctuatorType::ARROW:
        case PunctuatorType::MEMBER_ACCESS: {
                int index = creator->computeIndex(this->lhs, this->rhs);
                if(this->op == PunctuatorType::ARROW)
                       return creator->getAddressfromPointer(lhs,rhs,index);
               return creator->getMemberAddress(lhs,rhs, index);
                                            }
        case PunctuatorType::ARRAY_ACCESS:
               return creator->getArrayPosition(lhs,rhs, 0);//FIXME
        default:
               throw CompilerException("INTERNAL ERROR", this->pos());
        }
}
/*
 * Produces the corresponding rvalue. First the rvalue of the operand is 
 * computed, then the operator is applied.
 */
EMIT_RV(Parsing::UnaryExpression) {
  llvm::Value* vl = this->operand->emit_rvalue(creator);
  switch(this->op){
	case PunctuatorType::NOT:
	        creator->createLogNeg(vl);
	case PunctuatorType::MINUS:
		creator->createNeg(vl);
	case PunctuatorType::STAR:
		creator->createDeref(vl);
	case PunctuatorType::AMPERSAND:
		creator->createAddress(vl);
	default:
		throw CompilerException("INTERNAL ERROR", this->pos());
  }
  return nullptr;
}

/*
 * A unary operator can be a valid lvalue, so we need to allow this for code 
 * generation. First compute the lvalue of the operand and then apply the 
 * operator. Corresponding value is returned.
 */
EMIT_LV(Parsing::UnaryExpression) {
  llvm::Value* vl  = this->operand->emit_lvalue(creator);

  switch(this->op){
        case PunctuatorType::STAR:
                  return creator->getDeref(vl);
        case PunctuatorType::AMPERSAND:
                  return creator->getAddress(vl);
        default:
                  throw CompilerException("INTERNAL ERROR", this->pos());
  }
  return nullptr;
}

/* 
 * Produces the rvalue of a variable by returning the variable name that llvm 
 * gave it so that we can do computations with it
 */
EMIT_RV(Parsing::VariableUsage) {
  auto address = this->emit_lvalue(creator);
  return creator->loadVariable(address);
}

/*
 * Computes a variables lvalue by returning the variable, that llvm has 
 * produced when it has been declared.
 */
EMIT_LV(Parsing::VariableUsage) {
  UNUSED(creator);
  auto type = this->getType();
  auto a = type->associatedValue;
  if (!a) {
    throw CompilerException(type->toString(), this->pos());
  }
  return a;
}

/*
 * Produces the rvalue of a literal by returning its variable.
 */
EMIT_RV(Parsing::Literal) {
  return creator->allocLiteral(this->name);
}

/*
 * Produces the constants value. It has been validated befory by the semantics
 * so we can advise llvm to create a new constant if it does not exist and 
 * return the value
 */
EMIT_RV(Parsing::Constant) {
  switch(this->ct){
          case Lexing::ConstantType::CHAR:
                  return creator->allocChar(this->name);
          case Lexing::ConstantType::INT:
                  return creator->allocInt(this->name);
          case Lexing::ConstantType::NULLPOINTER:
                  return creator->allocNullptr(this->name);
          default:
                  throw ParsingException("Other constants are handled in different classes!", pos());
  }
}

/*
 * A function call can produce a valid value. So we need to evaluate all 
 * parameters and then create the corresponding function call.
 */
EMIT_RV(Parsing::FunctionCall) {
 llvm::Value* func = this->funcName->emit_lvalue(creator);
  std::vector<llvm::Value*> values = std::vector<llvm::Value*> ();
  for(auto it = this->arguments.begin() ; it != this->arguments.end(); ++it){
        values.push_back((*it)->emit_rvalue(creator));
  }
  return creator->createFCall(func, values);
}

/*
 * A function can return the address of some valid space so it can also produce
 * a valid rvalue. this has been validated by the semantics. We only need
 * to compute the value and then return it.
 */
EMIT_LV(Parsing::FunctionCall) {
  llvm::Value* func = this->funcName->emit_lvalue(creator);
  std::vector<llvm::Value*> values = std::vector<llvm::Value*> ();
  for(auto it = this->arguments.begin() ; it != this->arguments.end(); ++it){
        values.push_back((*it)->emit_rvalue(creator));
  }
  return creator->createFCall(func, values);
}

/*
 * A ternary operator can produce a valid rvalue. First evaluate the condition.
 * Then return the value based on the condition.
 */
EMIT_RV(Parsing::TernaryExpression) {
  return creator->makeSelect(this->condition,this->lhs, this->rhs);
}

/*
 * Produce the ternary expressions value. Remember that both expressions need to
 * be evaluated as one could contain side effects.
 */
EMIT_LV(Parsing::TernaryExpression) {
  return creator->makeSelectLV(this->condition,this->lhs,this->rhs);
}

/*
 * Produces the rvalue of the sizeof expression. TODO!
 */
EMIT_RV(Parsing::SizeOfExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}
