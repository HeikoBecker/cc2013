#include "../parser/parser.h"
#include "labelgen.h"

namespace Codegeneration {
  LabelGen* labelgen;
  void genLLVMIR(const char* filename, Parsing::AstRoot root);
}
