#ifndef CODEGEN_SCCP_PASS_H
#define CODEGEN_SCCP_PASS_H
#pragma once

#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/InstVisitor.h"

#include <deque>
#include <limits>

enum LatticeState : unsigned char {
  top = std::numeric_limits<unsigned char>::max(),
  value = std::numeric_limits<unsigned char>::max() / 2,
  bottom = 0
};
static_assert(bottom < value && value < top, "Lattice must have a total order!");
struct ConstantLattice
{
  int value;
  LatticeState state;
};
struct Reachability {
  LatticeState state;
};
constexpr Reachability unreachable { bottom };
constexpr Reachability reachable { top };
constexpr ConstantLattice unknown { 0, bottom };

class ConstantTable : public std::map<llvm::Value*, ConstantLattice>
{
  public:
    void checkedInsert(std::pair<llvm::Value*, ConstantLattice> pair);
};

class BlockTable : public std::map<llvm::BasicBlock*, Reachability>
{
  public:
    void checkedInsert(std::pair<llvm::BasicBlock*, Reachability> pair);
};

struct SCCP_Pass : public llvm::FunctionPass {
  static char ID;
  std::vector<llvm::Value*>globals;

  SCCP_Pass();

  bool runOnFunction(llvm::Function &F) override;
  bool doInitialization(llvm::Module& M) override;
};

struct Transition: public llvm::InstVisitor<Transition, void> {
  
  ConstantTable constantTable;
  BlockTable blockTable;

  Transition(llvm::Function& F,
  BlockTable& blockTable,
  std::vector<llvm::Value*> globals);
  llvm::BasicBlock* currentBlock;

  llvm::BasicBlock* getNextBlock();
  
  //override each instruction here
  void visitAllocaInst(llvm::AllocaInst& alloc);
  void visitBinaryOperator (llvm::BinaryOperator& binOp);
  void visitCallInst(llvm::CallInst& call);
  void visitCastInst(llvm::CastInst& cast);
  void visitGetElementPtrInst(llvm::GetElementPtrInst& gep);
  void visitZExtInst(llvm::ZExtInst& zext);
  void visitSExtInst(llvm::SExtInst& sext);
  void visitTruncInst(llvm::TruncInst& trunc);
  void visitICmpInst(llvm::ICmpInst& icmp);
  void visitLoadInst(llvm::LoadInst& load);
  void visitStoreInst(llvm::StoreInst& store);
  void visitPHINode(llvm::PHINode& phi);
  void visitBranchInst(llvm::BranchInst& branch);
  void visitReturnInst(llvm::ReturnInst& ret);
  
  void enqueueCFGSuccessors(llvm::Instruction& inst);
  
  void deleteDeadBlocks();
  void tearDownInsts();
  //helper functions
  ConstantLattice getConstantLatticeElem(llvm::Value* val);
  Reachability getReachabilityElem(llvm::BasicBlock* block);
private:
  std::deque<llvm::BasicBlock*> workQueue;
};
#endif
