#include "sccp_pass.h"

#include "../utils/util.h"

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"
// contains ReplaceInstWithValue and ReplaceInstWithInst
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
//to get the macros for easy predecessor and successor iteration
#include "llvm/Support/CFG.h"

#include <queue>
#include <map>
#include <algorithm>

#define TRANSITION(X, Y) void Transition::X(Y)
#define BINOP llvm::Instruction::BinaryOps

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
  currBlock = workQueue.front();
  workQueue.pop(); //remove it from the queue
  return currBlock;
}

/*
 * TODO: Manipulate the lattice based on the operands
 */
TRANSITION(visitBinaryOperator, llvm::BinaryOperator& binOp) {
  //take the first two operands more should not be present
  llvm::Value *lhs= binOp.getOperand(0);
  llvm::Value *rhs= binOp.getOperand(1);

  //and the parent BasicBlock
  llvm::BasicBlock* parent = binOp.getParent();

  UNUSED(parent);

  //the old info computed before
  //FIXME: is this valid?
  ConstantLattice oldInfo = (* this->constantTable.find(&binOp)).second;
  ConstantLattice newInfo;


  //take the corresponding lattice values
  //we do not need to check wether find returns table.end() as this would mean 
  //there was some failure previously so wen want to crash here 
  //FIXME: Maybe add error message
  ConstantLattice lhsInfo = (*this->constantTable.find(lhs)).second;
  ConstantLattice rhsInfo = (*this->constantTable.find(rhs)).second;

  //check wether one value already is top so we cannot compute something
  if(lhsInfo.state == LatticeState::top || rhsInfo.state == LatticeState::top){
    //check if we have an And or an Or Instruction which we could evaluate 
    //short circuit:
    if(binOp.getOpcode() == BINOP::Or){
      //check if one of the values is != 0 so the whole exp is true
      if((lhsInfo.state != LatticeState::top && lhsInfo.value != 0) ||
         (rhsInfo.state != LatticeState::top && rhsInfo.value != 0)){
        newInfo.state = LatticeState::value;
        newInfo.value = 1;
    }else if(binOp.getOpcode() == BINOP::And) {
      //check if one value is 0 the case that both are != 0 can't occur here
      //as we know that at least one is top
      if((lhsInfo.state != LatticeState::top && lhsInfo.value == 0) ||
         (rhsInfo.state != LatticeState::top && rhsInfo.value == 0)){
        newInfo.state = LatticeState::value;
        newInfo.value = 0;
      }
    }else{
      // we really can set the value to top
      newInfo.state = LatticeState::top; 
    }
    //check for value modification
    if(newInfo.state != oldInfo.state || newInfo.value != oldInfo.value){ 
      this->constantTable.insert(std::pair<llvm::Value*,ConstantLattice>(&binOp,newInfo));
      this->enqueueCFGSuccessors(binOp); //add succesors to queue as we need to 
                                         //update them
    }
      return;
    }
  }
  
  //none of our operands is a top element --> we know that both have a fixed 
  //value. If one would be bottom, there would be a path from start to this
  //instruction where the operand is not defined --> SSA violated
  newInfo.state = LatticeState::value;

  switch(binOp.getOpcode()){
  case BINOP::Add:
    newInfo.value = lhsInfo.value + rhsInfo.value;
    break;
  case BINOP::Sub:
    newInfo.value = lhsInfo.value - rhsInfo.value;
    break;
  case BINOP::Mul:
    newInfo.value = lhsInfo.value * rhsInfo.value;
    break;
  case BINOP::Or:
    newInfo.value = lhsInfo.value || rhsInfo.value;
    break;
  case BINOP::And:
    newInfo.value = lhsInfo.value && rhsInfo.value;
    break;
  default:
    break; 
  }
  //check if the value was computed before
  if(newInfo.value != oldInfo.value){
    this->constantTable.insert(std::pair<llvm::Value*,ConstantLattice>(&binOp,newInfo));
    //enqueue the successors for updating them
    this->enqueueCFGSuccessors(binOp);
  }
  return;
}

/*
 * TODO: Make this one an on-the-fly optimize function as it does not affect the
 * lattice but casts to already achieved type can be removed
 */
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

void Transition::enqueueCFGSuccessors(llvm::Instruction &inst){
  llvm::BasicBlock* parent = inst.getParent();
  for(auto block=succ_begin(parent); block != succ_end(parent); ++block){
    //once again end should not be possible FIXME: Add internal error diag
    Reachability reach = (*this->blockTable.find(*block)).second;
    if(reach.state == LatticeState::top) //Top means Reachable
      workQueue.push(*block);
  }
}
