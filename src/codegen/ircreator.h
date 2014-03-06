#ifndef IRCREATOR_H
#define IRCREATOR_H

#include "llvm/IR/Module.h"                /* Module */
#include "llvm/IR/Function.h"              /* Function */
#include "llvm/IR/Constant.h"              /* Constant::getNullValue */
#include "llvm/IR/IRBuilder.h"             /* IRBuilder */
#include "llvm/IR/LLVMContext.h"           /* LLVMContext */
#include "llvm/IR/GlobalValue.h"           /* GlobaleVariable, LinkageTypes */
#include "llvm/Analysis/Verifier.h"        /* verifyFunction, verifyModule */
#include "llvm/Support/raw_ostream.h"

#include <memory>

namespace Parsing {
  class Expression;
  class SemanticDeclaration;
  typedef std::shared_ptr<Expression> SubExpression;
  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;
}


//convenience macros
#define BINEXPCREATE(X) llvm::Value* X(llvm::Value* lhs, llvm::Value* rhs);
#define BINEXPCRIND(X) llvm::Value* X(llvm::Value* lhs, llvm::Value* rhs,\
                int index);
#define UNEXPCREATE(X) llvm::Value* X(llvm::Value* vl);
#define ALLOC(X) llvm::Value* X(std::string name);


namespace Codegeneration {

        class IRCreator {
        
                public:
		IRCreator(const char* filename);
		~IRCreator();
                void print(llvm::raw_fd_ostream & out);
                // expressions
		BINEXPCREATE(createAdd)
                BINEXPCREATE(createPAdd)
		BINEXPCREATE(createMinus)
                BINEXPCREATE(createPMinus)
                BINEXPCREATE(createPPMinus)
		BINEXPCREATE(createLess)
		BINEXPCREATE(createMult)
		BINEXPCREATE(createUnequal)
		BINEXPCREATE(createEqual)
		BINEXPCREATE(createLogAnd)
		BINEXPCREATE(createLogOr)
                BINEXPCREATE(createArrayAccess)
		llvm::Value* createAssign(llvm::Value* lhs, llvm::Value* rhs,
                                llvm::Type* type);
		BINEXPCRIND(createPointerAccess)
		BINEXPCRIND(createAccess)
                BINEXPCRIND(getAddressfromPointer)
                BINEXPCRIND(getMemberAddress)
                BINEXPCRIND(getArrayPosition)
                UNEXPCREATE(createLogNeg)
                UNEXPCREATE(createNeg)
                UNEXPCREATE(createDeref)
                llvm::Value* createSizeof(llvm::Type*);
                // are the functions below needed FIXME
                llvm::Value* loadVariable(llvm::Value *val);
                ALLOC(allocLiteral)
                llvm::Value* allocChar(char val);
                llvm::Value* allocInt ( int val);
                llvm::Value* allocNullptr(llvm::Type*);
                llvm::Value* createFCall(llvm::Value* func, 
                                std::vector<llvm::Value*> params, 
                                std::vector<llvm::Type*> paramTypes);
                // controlflow
                llvm::BasicBlock* getControlFlowBlock();
                void setCurrentBasicBlock(llvm::BasicBlock*);
                llvm::Value* makeSelect(Parsing::SubExpression cond, 
                                Parsing::SubExpression lhs, 
                                Parsing::SubExpression rhs);
                llvm::Value* makeSelectLV(Parsing::SubExpression cond, 
                                Parsing::SubExpression lhs, 
                                Parsing::SubExpression rhs);
                void makeReturn(llvm::Value *value);
                // creates a new basic block for the if head and set an insert
                // point into it
                llvm::BasicBlock* makeIfHeader();
                // get the correct basic block

                // create a block 
                // connected : true -> begins with follow block
                // connected : false -> is independent block
                llvm::BasicBlock* makeBlock(std::string labelName, bool connect = true);

                // adds an unconditional jump from "from" to "to"
                // and returns from
                llvm::BasicBlock* connect(llvm::BasicBlock *from, llvm::BasicBlock *to);
                void connect(llvm::BasicBlock *to);
                void makeConditonalBranch(
                    llvm::Value* branchCondition,
                    llvm::BasicBlock* consequenceBlock,
                    llvm::BasicBlock* alternativeBlock
                );
                llvm::Value* makePhi(
                    llvm::BasicBlock* consequenceBlock,
                    llvm::Value* consequenceValue,
                    llvm::BasicBlock* alternativeBlock,
                    llvm::Value* alternativeValue
                    );
                // utilities
                llvm::Type* semantic_type2llvm_type(
                    const Parsing::SemanticDeclarationNode semantic_type);
                llvm::Value* allocateInCurrentFunction(llvm::Type* type);
                void store(llvm::Value* value, llvm::Value *ptr);
                llvm::Value* createLoad(llvm::Value* val);
                // functions
                /* Allocates a basic block for a functions,
                 * and sets the Builder to it
                 */
                llvm::Function *startFunction(
                    llvm::FunctionType* function_type,
                    std::string name,
                    bool definition=true
                    );
                llvm::Function *startAlreadyDefinedFunction(
                    llvm::Function* function,
                    std::string name
                    );

                
                void setCurrentBreakPoint(llvm::BasicBlock* block);
                void setCurrentContinuePoint(llvm::BasicBlock* block);


                /* Ensures that the last block of the function has a terminator
                 */
                void finishFunction();
                // declarations
                llvm::GlobalVariable *makeGlobVar(llvm::Type *type);
                int computeIndex (Parsing::SubExpression lhs,
                               Parsing::SubExpression rhs);

                void makeBreak();
                void makeContinue();
                llvm::BasicBlock* getCurrentBlock(); 

                bool hasLabel(std::string label);
                void addLabel(llvm::BasicBlock *block, std::string label);
                llvm::BasicBlock* getLabelBlock(std::string label);
                        
                //Method to do type conversion if necessary
                llvm::Value* convert(llvm::Value*, llvm::Type*);
                llvm::Value* convert(llvm::Value*, Parsing::SemanticDeclarationNode);

                //type field to quickly modify our "usual arithmetic 
                //conversions" type
                llvm::Type* USUALTYPE;
	private:
		llvm::Module M;
		llvm::IRBuilder<> Builder, AllocaBuilder;
                llvm::Function* currentFunction;
                llvm::BasicBlock* currentBreakPoint;
                llvm::BasicBlock* currentContinuePoint;
                std::map<std::string, llvm::BasicBlock* > mapLabel;
  };
}
#endif
