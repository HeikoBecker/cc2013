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


void Codegeneration::genLLVMIR(const char* filename, Parsing::AstRoot root) {

  auto &Ctx = llvm::getGlobalContext();
  std::string errorStr;
  llvm::raw_fd_ostream stream(filename, errorStr);
  llvm::Module* M = new llvm::Module(filename, Ctx);
  llvm::IRBuilder<>* Builder = new llvm::IRBuilder<>(Ctx);
  llvm::IRBuilder<>* AllocaBuilder = new llvm::IRBuilder<>(Ctx);
  Codegeneration::IRCreator Creator (M, Builder, AllocaBuilder);
  root->emitIR(&Creator);
  verifyModule(*M);
  (*M).print(stream, nullptr); /* M is a llvm::Module */
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
  
  // TODO: move global variable creato into creator method?
  GlobalVariable *GlobVar = new GlobalVariable(
          *creator->M                                      /* Module & */,
          external_declaration_type                              /* Type * */,
          false                                   /* bool isConstant */,
          GlobalValue::CommonLinkage              /* LinkageType */,
          llvm::Constant::getNullValue(external_declaration_type)      /* Constant * Initializer */,
          "TODO"                                /* const Twine &Name = "" */,
          /*--------- We do not need this part (=> use defaults) ----------*/
          0                                       /* GlobalVariable *InsertBefore = 0 */,
          GlobalVariable::NotThreadLocal          /* ThreadLocalMode TLMode = NotThreadLocal */,
          0                                       /* unsigned AddressSpace = 0 */,
          false                                   /* bool isExternallyInitialized = false */);
   //TODO: what should we do with the global variable now?
  GlobVar->setName(this->declarator->getIdentifier()); // FIXME: we probably want a get name method
  this->getSemanticNode()->associatedValue = GlobVar;
}

EMIT_IR(Parsing::FunctionDefinition)
{
  // TODO: make name a member of functiondefiniton or declaration
  auto name = this->declarator->getIdentifier();
  // lookup the type of the current function
  auto semtree = Parsing::SemanticForest::filename2SemanticTree(this->pos().name);
  auto function_type = static_cast<llvm::FunctionType*>(creator->semantic_type2llvm_type(semtree->lookUpType(name, this->pos())));
  auto function = creator->startFunction(function_type, name);
  std::for_each(function->arg_begin(), function->arg_end(),
      [&](decltype(function->arg_begin()) argument){
      // 1. Allocate a stack slot
      auto ptr = creator->allocateInCurrentFunction(argument->getType());
      // 2. Store the argument value
      creator->store(argument, ptr);
  });
  
  // emit code for the body
  this->compoundStatement->emitIR(creator);
  creator->finishFunction();
}

EMIT_IR(Parsing::CompoundStatement)
{
  for (auto statement : this->subStatements) {
    statement->emitIR(creator);
  }
}

//##############################################################################
//#                    Expression Code Generation                              #
//##############################################################################

/*
 * Every expression has the emitIR function that redirects the call to 
 * emit_rvalue.
 */
EMIT_IR(Parsing::ExpressionStatement)
{
  this->expression->emitIR(creator);
}

/*
 * An expression can be part of a statement with e; where e is a statement.
 * So we need! emitIR to produce the rvalue.
 * The rvalue function will be overwritten by the corresponding class so we can
 * just call it here
 */
EMIT_IR(Parsing::Expression)
{
	this->emit_rvalue(creator);
}

/*
 * Methods for abstract expression object. They should never be called. If this
 * happens, we forgot to override the method at a specific place, that will be
 * printed by the exception.
 */
inline EMIT_RV(Parsing::Expression) {
  UNUSED(creator);
  throw CompilerException("You did not override the method emit_rvalue", this->pos());
}
inline EMIT_LV(Parsing::Expression) {
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
  // FIXME: not every operator requires lvalues! And the emit methods have side
  // effects, so they mustn't be called when the value is not required
  llvm::Value* lhs = nullptr;//this->lhs->emit_rvalue(creator);
  llvm::Value* rhs = nullptr;//this->rhs->emit_rvalue(creator);

  //then compute the corresponding value based on the operand
  //the creator will to the llvm magic. We just want to find the
  //correct method to call
  switch(this->op){
	case PunctuatorType::PLUS:
		return creator->createAdd(lhs, rhs);
	case PunctuatorType::MINUS:
		return creator->createMinus(lhs, rhs);
	case PunctuatorType::LESS:
		return creator->createLess(lhs, rhs);
	case PunctuatorType::STAR:
		return creator->createMult(lhs, rhs);
	case PunctuatorType::NEQUAL:
		return creator->createUnequal(lhs, rhs);
	case PunctuatorType::EQUAL:
		return creator->createEqual(lhs, rhs);
	case PunctuatorType::LAND:
		return creator->createLogAnd(lhs,rhs);
	case PunctuatorType::LOR:
		return creator->createLogOr(lhs, rhs);
	case PunctuatorType::ARROW: {
                Parsing::SemanticDeclarationNode type = this->lhs->getType();
                auto  structtype = std::static_pointer_cast<StructDeclaration> (type);
                auto it = structtype->members().begin();
                int i = 0;
                while (it->second != this->rhs->getType()){
                        ++it;
                        ++i;
                }
		return creator->createPointerAccess(lhs, rhs, 
                                this->rhs->getType());
                                    }
	case PunctuatorType::MEMBER_ACCESS:
		return creator->createAccess(lhs, rhs, this->rhs->getType());
	case PunctuatorType::ASSIGN:
                lhs = this->lhs->emit_lvalue(creator);
                rhs = this->rhs->emit_rvalue(creator);
		return creator->createAssign(lhs,rhs, this->rhs->getType());
	default:
	  throw CompilerException("INTERNAL ERROR", this->pos());
	}
}

EMIT_LV(Parsing::BinaryExpression){
        llvm::Value* lhs = this->lhs->emit_lvalue(creator);
        llvm::Value* rhs = this->rhs->emit_lvalue(creator);

        switch (this->op){
        case PunctuatorType::ARROW:
               return creator->getAddressfromPointer(lhs,rhs, 
                               this->rhs->getType());
        case PunctuatorType::MEMBER_ACCESS:
               return creator->getMemberAddress(lhs,rhs, this->rhs->getType()); 
        case PunctuatorType::ARRAY_ACCESS:
               return creator->getArrayPosition(lhs,rhs, this->rhs->getType());
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
EMIT_RV(Parsing::VariableUsage) { //FIXME use the field Fabian is adding in branch cogen_experimental
  return creator->loadVariable(this->getType(), this->name);
}

/*
 * Computes a variables lvalue by returning the variable, that llvm has 
 * produced when it has been declared.
 */
EMIT_LV(Parsing::VariableUsage) {
  /* TODO: why don't we just put this generic case, loading from rvalue in the
   * parent? */
  return creator->createLoad(this->emit_rvalue(creator));
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
  }
}

/*
 * A function call can produce a valid value. So we need to evaluate all 
 * parameters and then create the corresponding function call.
 */
EMIT_RV(Parsing::FunctionCall) {
 llvm::Value* func = this->funcName->emit_lvalue(creator);
  std::vector<llvm::Value*>* values = new std::vector<llvm::Value*> ();
  for(auto it = this->arguments.begin() ; it != this->arguments.end(); ++it){
        values->push_back((*it)->emit_rvalue(creator));
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
  std::vector<llvm::Value*>* values = new std::vector<llvm::Value*> ();
  for(auto it = this->arguments.begin() ; it != this->arguments.end(); ++it){
        values->push_back((*it)->emit_rvalue(creator));
  }
  return creator->createFCall(func, values);
}

/*
 * A ternary operator can produce a valid rvalue. First evaluate the condition.
 * Then return the value based on the condition.
 * TODO: Use the phi trick that was shown in the slides!
 */
EMIT_RV(Parsing::TernaryExpression) {
  llvm::Value* cond = this->condition->emit_rvalue(creator);
  llvm::Value* lhs = this->lhs->emit_rvalue(creator);
  llvm::Value* rhs = this->rhs->emit_rvalue(creator);
  return creator->makeSelect(cond,lhs,rhs);
}

/*
 * Produce the ternary expressions value. Remember that both expressions need to
 * be evaluated as one could contain side effects.
 * TODO: Use the phi trick that was shown in the slides!
 */
EMIT_LV(Parsing::TernaryExpression) {
  llvm::Value* cond = this->condition->emit_rvalue(creator);
  llvm::Value* lhs = this->lhs->emit_lvalue(creator);
  llvm::Value* rhs = this->rhs->emit_lvalue(creator);
  return creator->makeSelect(cond,lhs,rhs);
}

/*
 * Produces the rvalue of the sizeof expression. TODO!
 */
EMIT_RV(Parsing::SizeOfExpression) {
  UNUSED(creator); //FIXME
  return nullptr;
}
