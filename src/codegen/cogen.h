#ifndef COGEN_H
#define COGEN_H
#include <memory>

namespace Parsing {
  class AstNode;
  class SemanticDeclaration;
  typedef std::shared_ptr<AstNode> AstRoot;
  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;
}

namespace Codegeneration {
  void genLLVMIR(const char* filename, Parsing::AstRoot root, bool optimize=false);
}

#endif
