#include "cogen.h"
#include "../parser/astNode.h"
#include "../parser/semantic.h"

#include <memory>

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
  (void) M;
}

void Parsing::TranslationUnit::emitIR(llvm::Module & M)
{
  // call emitIR on each external declaration of the translation unit
  for (auto external_declaration: this->externalDeclarations) {
    external_declaration->emitIR(M);
  }
}

void Parsing::ExternalDeclaration::emitIR(llvm::Module & M)
{
  // TODO: implement this
  (void) M;
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
  (void) return_type_; // FIXME: marked as unused for now
  auto return_type = (llvm::Type *) Builder.getInt32Ty(); //FIXME
  /*************************************/
  /* TODO: set the correct parameter types */
  auto parameter_types = std::vector<llvm::Type *>(function_type_->parameter().size());
  // iterate over parameter_types and push corresponding LLVM type into vector
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
  (void) argument_it;
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
