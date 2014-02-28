#ifndef COGEN_H
#define COGEN_H
#include "../parser/parser.h"

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"



namespace Codegeneration {
  void genLLVMIR(const char* filename, Parsing::AstRoot root);

  class IRCreator {
  
	public:
		IRCreator(llvm::Module* M, llvm::IRBuilder<>* Builder, 
				llvm::IRBuilder<>* AllocaBuilder);
		~IRCreator();
		llvm::Value* createAdd(llvm::Value* lhs, llvm::Value* rhs);
	private:
		llvm::Module* M;
		llvm::IRBuilder<>* Builder, * AllocaBuilder;

		llvm::Type* semantic_type2llvm_type(
                    const Parsing::SemanticDeclarationNode semantic_type);
  };

}

#endif
