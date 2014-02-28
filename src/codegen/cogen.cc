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

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

/*Code below is from LLVM's Kaleiscope tutorial*/
/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
/*static*/ llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                llvm::Type *type,
                                                const std::string &VarName)
{
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                 TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, 0, VarName.c_str());
}

void Codegeneration::genLLVMIR(const char* filename, Parsing::AstRoot root) {

  auto &Ctx = llvm::getGlobalContext();
  std::string errorStr;
  llvm::raw_fd_ostream stream(filename, errorStr);
  llvm::Module M(filename, Ctx);
  root->emitIR(M);
  verifyModule(M);
  M.print(stream, nullptr); /* M is a llvm::Module */
}

void Parsing::AstNode::emitIR(llvm::Module & M)
{
  std::cout << "TODO\n";
  UNUSED(M);
}

void Parsing::TranslationUnit::emitIR(llvm::Module & M)
{
  // call emitIR on each external declaration of the translation unit
  for (auto external_declaration: this->externalDeclarations) {
    external_declaration->emitIR(M);
  }
}



/*
 *  Converts one of our type classes to the corresponding llvm Type
 */
llvm::Type* semantic_type2llvm_type(
    llvm::IRBuilder<> Builder,
    Parsing::SemanticDeclarationNode const semantic_type) {
  llvm::Type *llvm_type = nullptr;
  switch(semantic_type->type()){
    case Semantic::Type::INT:
      llvm_type = Builder.getInt32Ty();
      break;

    case Semantic::Type::CHAR:
      llvm_type = Builder.getInt8Ty();
      break;
                              
    case Semantic::Type::VOID:
      llvm_type = Builder.getVoidTy();
      break;
                              
    case Semantic::Type::POINTER:
      {
        auto pointer_type =
          std::static_pointer_cast<Parsing::PointerDeclaration>(semantic_type);	
        llvm_type = llvm::PointerType::getUnqual(
            semantic_type2llvm_type(Builder, pointer_type->pointee())
            );
      break;
      }
                                 
    case Semantic::Type::ARRAY:
      // TODO
      break;
                               
    case Semantic::Type::FUNCTION:
      // Should we handle functions here?
      break;
    case Semantic::Type::STRUCT:
      {
        auto structType =
          std::static_pointer_cast<Parsing::StructDeclaration>(semantic_type);
        UNUSED(structType);
        break;
      }
  }
  return llvm_type;
}

void Parsing::ExternalDeclaration::emitIR(llvm::Module & M)
{
  // TODO: type mapping needs to go into a different method!
  using namespace llvm;
  // we either have to work with a global declaration or a forward declaration
  // first we take the type of the node
  auto Builder = llvm::IRBuilder<>(M.getContext());

  llvm::Type * external_declaration_type;
  switch(this->declNode->type()){
    case Semantic::Type::FUNCTION:
      //We have reached a forward declaration which is not needed in 
      //llvm --> just skip it
      // TODO: this is probably not true
      return;
    default:
      external_declaration_type = semantic_type2llvm_type(Builder, this->declNode);
    
  }
  GlobalVariable *GlobVar = new GlobalVariable(
          M                                       /* Module & */,
          external_declaration_type                              /* Type * */,
          false                                   /* bool isConstant */,
          GlobalValue::CommonLinkage              /* LinkageType */,
          llvm::Constant::getNullValue(external_declaration_type)      /* Constant * Initializer */,
          "TODO"                                /* const Twine &Name = "" */,
  /* --------- We do not need this part (=> use defaults) ---------- */
          0                                       /* GlobalVariable *InsertBefore = 0 */,
          GlobalVariable::NotThreadLocal          /* ThreadLocalMode TLMode = NotThreadLocal */,
          0                                       /* unsigned AddressSpace = 0 */,
          false                                   /* bool isExternallyInitialized = false */);
  // TODO: what should we do with the global variable now?
  GlobVar->setName(this->declarator->getIdentifier()); // FIXME: we probably want a get name method
}

void Parsing::FunctionDefinition::emitIR(llvm::Module & M) {
  // TODO: make name a member of functiondefiniton or declaration
  auto name = this->declarator->getIdentifier();
  // lookup the type of the current function
  auto semtree = Parsing::SemanticForest::filename2SemanticTree(this->pos().name);
  auto function_type_ = std::static_pointer_cast<FunctionDeclaration>(semtree->lookUpType(name, this->pos()));
  auto Builder = llvm::IRBuilder<>(M.getContext());
  // lookup the return type and set it correctly
  auto return_type_ = function_type_->returnType();
  /* TODO: set the correct return type */
  UNUSED(M);
  auto return_type = (llvm::Type *) Builder.getInt32Ty(); //FIXME
  /*************************************/
  /* TODO: set the correct parameter types */
  auto parameter_types = std::vector<llvm::Type *>(function_type_->parameter().size());
  // iterate over parameter_types and push corresponding LLVM type into vector
  std::transform(
      function_type_->parameter().cbegin(),
      function_type_->parameter().cend(),
      begin(parameter_types),
      [&](SemanticDeclarationNode s) {
        return semantic_type2llvm_type(Builder, s);
  });
  /*************************************/
  llvm::FunctionType* function_type = llvm::FunctionType::get(
      return_type,
      parameter_types,
      /*isVarArg=*/false); 
  auto function = llvm::Function::Create(
      function_type,
      llvm::GlobalValue::ExternalLinkage,
      name,
      &M
      );
  /* TODO: set the names of the function arguments
   * byiterating over them and calling setName*/
  // TODO: retrive function argument names
  auto argument_it = function->begin();
  UNUSED(argument_it);
  /********************************************/
  // Create the basic block for the function
  auto function_basic_block = llvm::BasicBlock::Create(
      M.getContext(),
      name + "entry",
      function,
      0 //InsertBefore: inserts at end of surrounding function?
      );
  Builder.SetInsertPoint(function_basic_block);
  /* TODO: store each argument on the stack
   * 1. Allocate a stack slot
   */
  /*
   * 2. Store the argument value
   */
  // emit code for the body
  this->compoundStatement->emitIR(M);

  // stol^H^H^H^H borrowed from Johannes' example
  /* All code was emitted,.. but the last block might be empty.
   * If the last block does not end with a terminator statement the simple
   * rules created either dead code or the function is a void function without
   * a return on each path. Either way we need to add a terminator instruction
   * to the last block. The idea is to look at the return type of the current
   * function and emit either a void return or a return with the 'NULL' value
   * for this type */
  if (Builder.GetInsertBlock()->getTerminator() == nullptr) {
    auto CurFuncReturnType = Builder.getCurrentFunctionReturnType();
    if (CurFuncReturnType->isVoidTy()) {
      Builder.CreateRetVoid();
    } else {
      Builder.CreateRet(llvm::Constant::getNullValue(CurFuncReturnType));
    }
  }
}

void Parsing::CompoundStatement::emitIR(llvm::Module & M) {
  for (auto statement : this->subStatements) {
    statement->emitIR(M);
  }
}

void Parsing::ExpressionStatement::emitIR(llvm::Module & M) {
  this->expression->emit_rvalue(M);
}

void Parsing::Expression::emitIR(llvm::Module & M) {
  UNUSED(M);
  std::cerr << "Don't call emitIR of an expression!\n"
            << "Either call emit_rvalue or emit_lvalue\n";
}

llvm::Value* Parsing::Expression::emit_rvalue(llvm::Module & M) {
  // get lvalue of Variable
  auto address = this->emit_lvalue(M);
  UNUSED(address);
  // load from lvalue
  return nullptr; // FIXME
}


llvm::Value* Parsing::Expression::emit_lvalue(llvm::Module & M) {
  UNUSED(M);
  if (this->can_be_lvalue()) {
    throw CompilerException("Not implemeted!", this->pos());
  }
  throw CompilerException("Illegal LVALUE!", this->pos());
}

/*
 * Emits the complete value calculation for the expression. As statements of the
 * form e; where e is an expression are valid C, we just explicitely call the 
 * RVALUE generating function here and ignore the result value
 */
void Parsing::BinaryExpression::emitIR(llvm::Module & M) {
  this->emit_rvalue(M);
}

/*
 * Creates the RVALUE of the BinaryExpression object and returns it for further
 * usage. First compute left and right values, then emit the instruction based 
 * on the operand
 */
llvm::Value* Parsing::BinaryExpression::emit_rvalue(llvm::Module & M){
  UNUSED(M);//FIXME
  return nullptr;
}

/*
 * A unary operator can be a valid lvalue, so we need to allow this for code 
 * generation. //TODO: Determine correct operators or not?
 * First compute the lvalue of the operand and then apply the operator.
 * Corresponding value is returned.
 */
llvm::Value* Parsing::UnaryExpression::emit_lvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Produces the corresponding rvalue. First the rvalue of the operand is 
 * computed, then the operator is applied.
 */
llvm::Value* Parsing::UnaryExpression::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/* 
 * Produces the rvalue of a variable by returning the variable name that llvm 
 * gave it so that we can do computations with it
 */
llvm::Value* Parsing::VariableUsage::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Computes a variables lvalue by returnning the vartiable, that llvm has 
 * produced when it has been declared.
 */
llvm::Value* Parsing::VariableUsage::emit_lvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Produces the rvalue of a literal by returning its variable.
 */
llvm::Value* Parsing::Literal::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Produces the lvalue of a literal, as string literals are arrays and therefore
 * they have an address where they are stored.
 */
llvm::Value* Parsing::Literal::emit_lvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Produces the constants value. It has been validated befory by the semantics
 * so we can advise llvm to create a new constant if it does not exist and 
 * return the value
 */
llvm::Value* Parsing::Constant::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * A function call can produce a valid value. So we need to evaluate all 
 * parameters and then create the corresponding function call.
 */
llvm::Value* Parsing::FunctionCall::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * A function can return the address of some valid space so it can also produce
 * a valid rvalue. this has been validated by the semantics. We only need
 * to compute the value and then return it.
 */
llvm::Value* Parsing::FunctionCall::emit_lvalue(llvm::Module & M){
 UNUSED(M); //FIXME
 return nullptr;
}

/*
 * A ternary operator can produce a valid rvalue. First evaluate the condition.
 * Then return the value based on the condition.
 * TODO: Use the phi trick that was shown in the slides!
 */
llvm::Value* Parsing::TernaryExpression::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}

/*
 * Produce the ternary expressions value. Remember that both expressions need to
 * be evaluated as one could contain side effects.
 * TODO: Use the phi trick that was shown in the slides!
 */
llvm::Value* Parsing::TernaryExpression::emit_lvalue(llvm::Module & M){
 UNUSED(M); //FIXME
 return nullptr;
}

/*
 * Produces the rvalue of the sizeof expression. TODO!
 */
llvm::Value* Parsing::SizeOfExpression::emit_rvalue(llvm::Module & M){
  UNUSED(M); //FIXME
  return nullptr;
}
