#ifndef COGEN_H
#define COGEN_H
#include <memory>

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

//convenience macros
#define EXPCREATE(X) llvm::Value* X(llvm::Value* lhs, llvm::Value* rhs);

namespace Parsing {
  class AstNode;
  class SemanticDeclaration;
  typedef std::shared_ptr<AstNode> AstRoot;
  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;
}



namespace Codegeneration {
  void genLLVMIR(const char* filename, Parsing::AstRoot root);

  class IRCreator {
  
	public:
		IRCreator(llvm::Module* M, llvm::IRBuilder<>* Builder, 
				llvm::IRBuilder<>* AllocaBuilder);
		~IRCreator();
		EXPCREATE(createAdd)
		EXPCREATE(createMinus)
		EXPCREATE(createLess)
		EXPCREATE(createMult)
		EXPCREATE(createUnequal)
		EXPCREATE(createEqual)
		EXPCREATE(createLogAnd)
		EXPCREATE(createLogOr)
		EXPCREATE(createPointerAccess)
		EXPCREATE(createAccess)
		EXPCREATE(createAssign)
		llvm::Module* M;
	private:
		llvm::IRBuilder<>* Builder, * AllocaBuilder;

		llvm::Type* semantic_type2llvm_type(
                    const Parsing::SemanticDeclarationNode semantic_type);
  };

}

#endif
