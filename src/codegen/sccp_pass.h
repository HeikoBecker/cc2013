#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/InstVisitor.h"

#include <queue>

namespace {
  enum LatticeState {
    top = 1,
    value,
    bottom = 0
  };
  struct ConstantLattice
  {
    int value;
    LatticeState state;
  };
  struct Reachability {
    LatticeState state;
  };
  constexpr Reachability unreachable { top };
  constexpr Reachability reachable { bottom };
  constexpr ConstantLattice unknown { 0, bottom };
}



struct SCCP_Pass : public llvm::FunctionPass {
  static char ID;
  SCCP_Pass();

  bool runOnFunction(llvm::Function &F) override;
};

struct Transition: public llvm::InstVisitor<Transition, void> {
  
  std::map<llvm::Value*, ConstantLattice> constantTable;
  std::map<llvm::BasicBlock*, Reachability> blockTable;

  Transition(llvm::BasicBlock* entryBlock,
  std::map<llvm::Value*, ConstantLattice>&  constantTable,
  std::map<llvm::BasicBlock*, Reachability>& blockTable);

  //FIXME: Maybe pass the queue to the sccp_pass instead of forcing invocation
  //of method
  llvm::BasicBlock* getNextBlock();
  
  //override each instruction here 
  //BinaryOperator
  void visitBinaryOperator (llvm::BinaryOperator& binOp);
  //CastInst
  void visitCastInst(llvm::CastInst& cast);
  //ICmpInst
  void visitICmpInst(llvm::ICmpInst& icmp);
  //PHINode
  void visitPHINode(llvm::PHINode& phi);
  //BranchInst
  void visitBranchInst(llvm::BranchInst& branch);
  //ReturnInst
  void visitReturnInst(llvm::ReturnInst& ret);
  
private:
  std::queue<llvm::BasicBlock*> workQueue;
  llvm::BasicBlock* currBlock; //to save the current block for equality checking
                               //to do queue management
};
