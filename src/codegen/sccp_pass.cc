#include "sccp_pass.h"

#include "../utils/util.h"

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"
// contains ReplaceInstWithValue and ReplaceInstWithInst
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <queue>
#include <map>
#include <algorithm>

#define TRANSITION(X, Y) void Transition::X(Y)

using namespace llvm;

SCCP_Pass::SCCP_Pass() : FunctionPass(ID) {}

bool SCCP_Pass::runOnFunction(llvm::Function &F) {
  // maps "registers" to values
  std::map<llvm::Value*, ConstantLattice> ValueMapping;

  // maps BasicBlocks to either reachable or unreachable
  std::map<llvm::BasicBlock*, Reachability> BlockMapping;

  std::for_each(
    F.begin(),
    F.end(),
    [&](llvm::Function::iterator function_basic_block) {
      BlockMapping[function_basic_block] = unreachable;
  });
  //llvm::ValueSymbolTable VT = F.getValueSymbolTable();
  //for (auto nameValuePair: VT) {
    //ValueMapping[nameValuePair.getValue()] = unknown;
  //}
 
  //Initialize the Transition object
  Transition transMngr = Transition(&F.getEntryBlock(), ValueMapping, BlockMapping);
  
  llvm::BasicBlock* curr = &F.getEntryBlock();

  do{
  //FIXME: Iterate over instructions of block here!
  }while ((curr = transMngr.getNextBlock()));

  return false;
}

char SCCP_Pass::ID = 0;
static RegisterPass<SCCP_Pass> X("sccp", "SCCP Pass", false, false);

//##############################################################################
//##                              Transition Class                            ##
//##############################################################################

Transition::Transition( llvm::BasicBlock *entryBlock,
                        std::map<llvm::Value*, ConstantLattice>& constantTable,
                        std::map<llvm::BasicBlock*, Reachability>& blockTable):
                        constantTable(constantTable), blockTable(blockTable) {
 //FIXME: Do we need to put all blocks in the queue to visit them once?
 currBlock = entryBlock;
}

/*
 * Removes the first element from the queue to give it to the sccppass to do the
 * work. As a side effect, the queues size is decreased by one when the element
 * is removed.
 */
llvm::BasicBlock* Transition::getNextBlock() {
  //save the first block to return it
  llvm::BasicBlock *res = workQueue.front();
  workQueue.pop(); //remove it from the queue
  currBlock = res;
  return res;
}

TRANSITION(visitBinaryOperator, llvm::BinaryOperator& binOp) {
  UNUSED(binOp);
  return;
}

TRANSITION(visitCastInst, llvm::CastInst &cast) {
  UNUSED(cast);
  return;
}

TRANSITION(visitICmpInst, llvm::ICmpInst &cmp){
  UNUSED(cmp);
  return;
}

TRANSITION(visitPHINode, llvm::PHINode &phi){
  UNUSED(phi);
  return;
}

TRANSITION(visitBranchInst, llvm::BranchInst &branch){
  UNUSED(branch);
  return;
}

TRANSITION(visitReturnInst, llvm::ReturnInst &ret){
  UNUSED(ret);
  return;
}
