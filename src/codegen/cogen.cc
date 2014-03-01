#include "cogen.h"
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

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

//convenience macros to save some typing time
//create is marked for being inlined!
#define EMIT_IR(X) void X::emitIR(Codegeneration::IRCreator* creator)
#define EMIT_LV(X) llvm::Value* X::emit_lvalue(Codegeneration::IRCreator* creator) 
#define EMIT_RV(X) llvm::Value* X::emit_rvalue(Codegeneration::IRCreator* creator) 
#define BINCREATE(X) inline llvm::Value* Codegeneration::IRCreator::X (llvm::Value* lhs, llvm::Value* rhs)
#define UNCREATE(X) inline llvm::Value* Codegeneration::IRCreator::X(llvm::Value* val)
#define ALLOCF(X) inline llvm::Value* Codegeneration::IRCreator::X(std::string name)

Codegeneration::IRCreator::IRCreator(llvm::Module* M, llvm::IRBuilder<>* Builder,
					llvm::IRBuilder<>* AllocaBuilder):
M(M), Builder(Builder), AllocaBuilder(AllocaBuilder) {
}	

llvm::Value *Codegeneration::IRCreator::allocateInCurrentFunction(llvm::Type* type)
{
    /* Reset the alloca builder each time before using it
   *
   *   It should not insert any instruction behind the terminator of the entry
   *   block, the easiest way to ensure this is to set it to the begining of
   *   the entry block each time we insert an alloca. */
  AllocaBuilder->SetInsertPoint(AllocaBuilder->GetInsertBlock(),
                               AllocaBuilder->GetInsertBlock()->begin());
  return AllocaBuilder->CreateAlloca(type);
}

llvm::Value* Codegeneration::IRCreator::createLoad(llvm::Value* val) {
  return Builder->CreateLoad(val);
}


void Codegeneration::IRCreator::store(llvm::Value* value, llvm::Value *ptr) {
  Builder->CreateStore(value,ptr);
}

llvm::Function* Codegeneration::IRCreator::startFunction(
    llvm::FunctionType* function_type,
    std::string name
)
{
  auto function = llvm::Function::Create(
      function_type,
      llvm::GlobalValue::ExternalLinkage,
      name,
      M
      );
  auto function_basic_block = llvm::BasicBlock::Create(
      M->getContext(), // FIXME: M
      name+"_begin",
      function,
      0 //InsertBefore: inserts at end of surrounding function?
      );
  Builder->SetInsertPoint(function_basic_block);
  AllocaBuilder->SetInsertPoint(function_basic_block);
  return function;
}

void Codegeneration::IRCreator::finishFunction()
{
  // stol^H^H^H^H borrowed from Johannes' example
  /* All code was emitted,.. but the last block might be empty.
   * If the last block does not end with a terminator statement the simple
   * rules created either dead code or the function is a void function without
   * a return on each path. Either way we need to add a terminator instruction
   * to the last block. The idea is to look at the return type of the current
   * function and emit either a void return or a return with the 'NULL' value
   * for this type */
  if (Builder->GetInsertBlock()->getTerminator() == nullptr) {
    auto CurFuncReturnType = Builder->getCurrentFunctionReturnType();
    if (CurFuncReturnType->isVoidTy()) {
      Builder->CreateRetVoid();
    } else {
      Builder->CreateRet(llvm::Constant::getNullValue(CurFuncReturnType));
    }
  }
}

Codegeneration::IRCreator::~IRCreator(){
UNUSED(AllocaBuilder); //FIXME
}

/*
 * Self explanatory binary expression functions. Special cases are annotated.
 */
BINCREATE(createAdd) {
	return Builder->CreateAdd(lhs,rhs);
}

BINCREATE(createMinus) {
	return Builder->CreateSub(lhs, rhs);
}

/*
 * We use signed less than for the less than operator as usual arithmetic conver
 * sions imply an implicit threatment of all values as i32 integer which are
 * signed in our C subset
 */
BINCREATE(createLess) {
	return Builder->CreateICmpSLT(lhs,rhs);
}

BINCREATE(createMult) {
	return Builder->CreateMul(lhs, rhs);
}

BINCREATE(createUnequal){
	return Builder->CreateICmpNE(lhs,rhs);
}

BINCREATE(createEqual){
	return Builder->CreateICmpEQ(lhs,rhs);
}

BINCREATE(createLogAnd){
	return Builder->CreateAnd(lhs, rhs);
}

BINCREATE(createLogOr){
	return Builder->CreateOr(lhs,rhs);
}

BINCREATE(createPointerAccess) { //FIXME
	UNUSED(lhs);
	UNUSED(rhs);
	return nullptr;
}

BINCREATE(createAccess) { //FIXME
	UNUSED(lhs);
	UNUSED(rhs);
	return nullptr;
}

BINCREATE(createAssign) { //FIXME
  store(lhs,rhs);
  return lhs;
}

BINCREATE(getAddressfromPointer){ //FIXME
        UNUSED(lhs);
        UNUSED(rhs);
        return nullptr;
}

BINCREATE(getMemberAddress){ //FIXME
        UNUSED(lhs);
        UNUSED(rhs);
        return nullptr;
}

BINCREATE(getArrayPosition) { //FIXME
        UNUSED(lhs);
        UNUSED(rhs);
        return nullptr;
}

UNCREATE(createLogNeg) { //FIXME
        UNUSED(val);
        return nullptr;
}

UNCREATE(createNeg) { //FIXME
        UNUSED(val);
        return nullptr;
}

UNCREATE(createDeref) { //FIXME
        UNUSED(val);
        return nullptr;
}

UNCREATE(createAddress) { //FIXME
        UNUSED(val);
        return nullptr;
}

UNCREATE(getDeref) { //FIXME
        UNUSED(val);
        return nullptr;
}

UNCREATE(getAddress) { //FIXME
        UNUSED(val);
        return nullptr;
}

llvm::Value* Codegeneration::IRCreator::loadVariable(
                Parsing::SemanticDeclarationNode type, 
                std::string name) { //FIXME
        UNUSED(type);
        UNUSED(name);
        return nullptr;
}

llvm::Value* Codegeneration::IRCreator::lookupVariable(
                Parsing::SemanticDeclarationNode type,
                std::string name) { //FIXME
        UNUSED(type);
        UNUSED(name);
        return nullptr;
}

ALLOCF(allocLiteral) { //FIXME
        UNUSED(name);
        return nullptr;
}

ALLOCF(allocChar) {//FIXME
        UNUSED(name);
        return nullptr;
}

ALLOCF(allocInt) { //FIXME
        UNUSED(name);
        return nullptr;
}

ALLOCF(allocNullptr) {//FIXME
        UNUSED(name);
        return nullptr;
}

llvm::Value* Codegeneration::IRCreator::createFCall(llvm::Value* func,
                std::vector<llvm::Value*>* params) { //FIXME
        UNUSED(func);
        UNUSED(params);
        return nullptr;
}

llvm::Value* Codegeneration::IRCreator::makeSelect(llvm::Value* cond, 
                llvm::Value* lhs, llvm::Value* rhs) { //FIXME
        UNUSED(cond);
        UNUSED(lhs);
        UNUSED(rhs);
        return nullptr;
}

/*
 *  Converts one of our type classes to the corresponding LLVM Type
 */
llvm::Type* Codegeneration::IRCreator::semantic_type2llvm_type(
    const Parsing::SemanticDeclarationNode semantic_type) {
  llvm::Type *llvm_type = nullptr;
  switch(semantic_type->type()){
    case Semantic::Type::INT:
      llvm_type = Builder->getInt32Ty();
      break;

    case Semantic::Type::CHAR:
      llvm_type = Builder->getInt8Ty();
      break;
                              
    case Semantic::Type::VOID:
      llvm_type = Builder->getVoidTy();
      break;
                              
    case Semantic::Type::ARRAY:
    case Semantic::Type::POINTER:
      {
        auto pointer_type =
          std::static_pointer_cast<Parsing::PointerDeclaration>(semantic_type);	
        llvm_type = llvm::PointerType::getUnqual(
            semantic_type2llvm_type(pointer_type->pointee())
            );
      break;
      }
                                 
    case Semantic::Type::STRUCT:
      {
        auto structType =
          std::static_pointer_cast<Parsing::StructDeclaration>(semantic_type);
        UNUSED(structType);
        llvm_type = llvm::StructType::create(this->Builder->getContext());
        /* TODO: handle non primitive types*/
        std::vector<llvm::Type *> member_types;
        // TODO: use transform
        for (auto member: structType->members()) {
          member_types.push_back(semantic_type2llvm_type(member.second));
        }
      }
    case Semantic::Type::FUNCTION:
      // Should we handle functions here?
    default:
      llvm_type = Builder->getInt32Ty();
  }
  return llvm_type;
}


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
  auto function_type_ = std::static_pointer_cast<FunctionDeclaration>(semtree->lookUpType(name, this->pos()));
  // lookup the return type and set it correctly
  auto return_type_ = function_type_->returnType();
  auto return_type = creator->semantic_type2llvm_type(return_type_); 
  /*************************************/
  /* TODO: set the correct parameter types */
  auto parameter_types = std::vector<llvm::Type *>();
  parameter_types.reserve(function_type_->parameter().size());
  // iterate over parameter_types and push corresponding LLVM type into vector
  for (auto p: function_type_->parameter()) {
    parameter_types.push_back(creator->semantic_type2llvm_type(p));
  }
  /*************************************/
  llvm::FunctionType* function_type = llvm::FunctionType::get(
      return_type,
      parameter_types,
      /*isVarArg=*/false); 
  /* TODO: set the names of the function arguments
   * byiterating over them and calling setName*/
  // TODO: retrive function argument names
  /********************************************/
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
	case PunctuatorType::ARROW:
		return creator->createPointerAccess(lhs, rhs);
	case PunctuatorType::MEMBER_ACCESS:
		return creator->createAccess(lhs, rhs);
	case PunctuatorType::ASSIGN:
                lhs = this->lhs->emit_lvalue(creator);
                rhs = this->rhs->emit_rvalue(creator);
		return creator->createAssign(lhs,rhs);
	default:
	  throw CompilerException("INTERNAL ERROR", this->pos());
	}
}

EMIT_LV(Parsing::BinaryExpression){
        llvm::Value* lhs = this->lhs->emit_lvalue(creator);
        llvm::Value* rhs = this->rhs->emit_lvalue(creator);

        switch (this->op){
        case PunctuatorType::ARROW:
               return creator->getAddressfromPointer(lhs,rhs);
        case PunctuatorType::MEMBER_ACCESS:
               return creator->getMemberAddress(lhs,rhs); 
        case PunctuatorType::ARRAY_ACCESS:
               return creator->getArrayPosition(lhs,rhs);
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
  UNUSED(creator); //FIXME
  return this->getType()->associatedValue;
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
