#ifndef COGEN_H
#define COGEN_H
#include <memory>
#include <string>

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

namespace Parsing {
  class AstNode;
  class SemanticDeclaration;
  typedef std::shared_ptr<AstNode> AstRoot;
  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;
}

namespace Codegeneration {
  void genLLVMIR(const char* filename, Parsing::AstRoot root);
}

#endif
