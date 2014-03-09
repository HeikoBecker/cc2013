#ifndef CODEGEN_SCCP_PASS_H
#define CODEGEN_SCCP_PASS_H
#pragma once

#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/InstVisitor.h"

#include <deque>

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



struct SCCP_Pass : public llvm::FunctionPass {
  static char ID;
  SCCP_Pass();

  bool runOnFunction(llvm::Function &F) override;
};

struct Transition: public llvm::InstVisitor<Transition, void> {
  
  std::map<llvm::Value*, ConstantLattice> constantTable;
  std::map<llvm::BasicBlock*, Reachability> blockTable;

  Transition(llvm::Function& F,
  std::map<llvm::BasicBlock*, Reachability>& blockTable);

  llvm::BasicBlock* getNextBlock();
  
  //override each instruction here
  void visitAllocaInst(llvm::AllocaInst& alloc);
  void visitBinaryOperator (llvm::BinaryOperator& binOp);
  void visitCallInst(llvm::CallInst& call);
  void visitCastInst(llvm::CastInst& cast);
  void visitGetElementPtrInst(llvm::GetElementPtrInst& gep);
  void visitICmpInst(llvm::ICmpInst& icmp);
  void visitLoadInst(llvm::LoadInst& load);
  void visitStoreInst(llvm::StoreInst& store);
  void visitPHINode(llvm::PHINode& phi);
  void visitBranchInst(llvm::BranchInst& branch);
  void visitReturnInst(llvm::ReturnInst& ret);
  
  void enqueueCFGSuccessors(llvm::Instruction& inst);
  
  //helper functions
  ConstantLattice getConstantLatticeElem(llvm::Value* val);
  Reachability getReachabilityElem(llvm::BasicBlock* block);
  
private:
  std::deque<llvm::BasicBlock*> workQueue;
};
#endif
