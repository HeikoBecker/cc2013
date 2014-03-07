#include "sccp_pass.h"


#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/raw_ostream.h"
#include <queue>
#include <map>
#include <algorithm>

using namespace llvm;

namespace {
  enum LatticeState {
    top = 1,
    value,
    bottom = 0
  };
  struct ConstantLattice
  {
    int value;
    LatticeState state = bottom;
  };
  struct Reachability {
    LatticeState state;
  };
  constexpr Reachability unreachable { top };
  constexpr Reachability reachable { bottom };
}

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
  return false;
}

char SCCP_Pass::ID = 0;
static RegisterPass<SCCP_Pass> X("sccp", "SCCP Pass", false, false);
