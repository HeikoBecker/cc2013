#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"

struct SCCP_Pass : public llvm::FunctionPass {
  static char ID;
  SCCP_Pass();

  bool runOnFunction(llvm::Function &F) override;
};
