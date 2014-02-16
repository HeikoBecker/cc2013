#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

#include "cogen.h"

using namespace llvm;

void Codegeneration::genLLVMIR(char* filename, Parsing::AstRoot root) {

  (void) root;
  LLVMContext &Ctx = getGlobalContext();
  std::string errorStr;
  raw_fd_ostream stream(filename, errorStr);
  Module M(filename, Ctx);
  M.print(stream, nullptr); /* M is a llvm::Module */
}

