#ifndef IRCREATOR_H
#define IRCREATOR_H

#include "../parser/parser.h"

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"



//convenience macros
#define BINEXPCREATE(X) llvm::Value* X(llvm::Value* lhs, llvm::Value* rhs);
#define BINEXPCREATEL(X) llvm::Value* X(llvm::Value* lhs, llvm::Value* rhs, Parsing::SemanticDeclarationNode rhsType);
#define UNEXPCREATE(X) llvm::Value* X(llvm::Value* vl);
#define ALLOC(X) llvm::Value* X(std::string name);


namespace Codegeneration {

        class IRCreator {
        
                public:
		IRCreator(llvm::Module* M, llvm::IRBuilder<>* Builder, 
				llvm::IRBuilder<>* AllocaBuilder);
		~IRCreator();
		BINEXPCREATE(createAdd)
		BINEXPCREATE(createMinus)
		BINEXPCREATE(createLess)
		BINEXPCREATE(createMult)
		BINEXPCREATE(createUnequal)
		BINEXPCREATE(createEqual)
		BINEXPCREATE(createLogAnd)
		BINEXPCREATE(createLogOr)
		BINEXPCREATEL(createPointerAccess)
		BINEXPCREATEL(createAccess)
		BINEXPCREATEL(createAssign)
                BINEXPCREATEL(getAddressfromPointer)
                BINEXPCREATEL(getMemberAddress)
                BINEXPCREATEL(getArrayPosition)
                UNEXPCREATE(createLogNeg)
                UNEXPCREATE(createNeg)
                UNEXPCREATE(createDeref)
                UNEXPCREATE(createAddress)
                UNEXPCREATE(getDeref)
                UNEXPCREATE(getAddress)
                llvm::Value* loadVariable(Parsing::SemanticDeclarationNode type,
                                std::string name);
                llvm::Value* lookupVariable(Parsing::SemanticDeclarationNode type,
                                std::string name);
                ALLOC(allocLiteral)
                ALLOC(allocChar)
                ALLOC(allocInt)
                ALLOC(allocNullptr)
                llvm::Value* createFCall(llvm::Value* func, 
                                std::vector<llvm::Value*>* params);
                llvm::Value* makeSelect(llvm::Value* cond, llvm::Value* lhs,
                                        llvm::Value* rhs);
		llvm::Module* M;
                llvm::Type* semantic_type2llvm_type(
                    const Parsing::SemanticDeclarationNode semantic_type);
                llvm::Value* allocateInCurrentFunction(llvm::Type* type);
                //void allocateAndStoreParameter(llvm::Type* type);
                /* Allocates a basic block for a functions,
                 * and sets the Builder to it
                 */
                void store(llvm::Value* value, llvm::Value *ptr);
                llvm::Value* createLoad(llvm::Value* val);
                llvm::Function *startFunction(
                    llvm::FunctionType* function_type,
                    std::string name
                    );
                /* Ensures that the last block of the function has a terminator
                 */
                void finishFunction();
	private:
		llvm::IRBuilder<>* Builder, * AllocaBuilder;

  };
}
#endif
