#ifndef PARSER_SEMADECL_H
#define PARSER_SEMADECL_H

#include <string>
#include <memory>
#include <vector>

namespace llvm {
  class Value;
  class Type;
}

namespace Semantic {
  enum class Type {
    INT,
    CHAR,
    VOID,
    POINTER,
    ARRAY,
    FUNCTION,
    STRUCT,
  };
}

namespace Parsing {

  class SemanticNode;
  typedef std::shared_ptr<SemanticNode> SubSemanticNode;

  class SemanticDeclaration {
    public: 
      virtual std::string toString();
      virtual Semantic::Type type();
      bool isVoid();
      llvm::Value* associatedValue = nullptr;
  };

  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;

  class IntDeclaration : public SemanticDeclaration {
    public: 
      Semantic::Type type() override;
      std::string toString() override;
  };

  // only used to distinguish null pointer value from integer
  class NullDeclaration : public IntDeclaration {
      std::string toString() override; 
  };

  class CharDeclaration : public SemanticDeclaration {
    public :
      Semantic::Type type() override;
      std::string toString() override;
  };

  class VoidDeclaration : public SemanticDeclaration {
    public: 
      std::string toString() override;
      Semantic::Type type() override;
  };

  class PointerDeclaration : public SemanticDeclaration {
    public:
      PointerDeclaration(int pointerCounter, Parsing::SemanticDeclarationNode type);
      Semantic::Type type() override;
      Parsing::SemanticDeclarationNode pointee();
      std::string toString() override;

    private:
      Parsing::SemanticDeclarationNode child;
  };

  /*
   *  an array derives from pointer
   *  this has the advantage that it can automatically decay into a pointer if
   *  a pointer_cast is used
   *
   *  we need the array to handle sizeof applied to string literals
   */
  class ArrayDeclaration : public PointerDeclaration {
    public:
      ArrayDeclaration(Parsing::SemanticDeclarationNode type, size_t size);
      // in type we are currently lying to faciliate the usage
      Semantic::Type type() override;
      std::string toString() override;
      const size_t size;
  };

  class FunctionDeclaration : public SemanticDeclaration {

    public:
      FunctionDeclaration(Parsing::SemanticDeclarationNode ret, std::vector<SemanticDeclarationNode> par); 
      Semantic::Type type() override;

      std::vector<Parsing::SemanticDeclarationNode> parameter();
      // Throws the parameter away and uses the provided vector for them
      // necessary to get the correct associatedValue
      // TODO: hacky. Why don't we use them directly?
      void rebindParameters(std::vector<Parsing::SemanticDeclarationNode>);
      Parsing::SemanticDeclarationNode returnType();
      std::string toString() override;

    private:
      Parsing::SemanticDeclarationNode returnChild;
      std::vector<Parsing::SemanticDeclarationNode> m_parameter;

  };

  class StructDeclaration : public SemanticDeclaration {

    public:  
      // name e.g. @S
      StructDeclaration(std::string n, SubSemanticNode s, bool selfReferencing);
      Semantic::Type type() override;
      std::string toString() override;
      bool isSelfReferencing();
      /* return members as a vector of name type pairs */
      std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>>
        members();
      SubSemanticNode node();
      // required to handle self-referencing structs in cogen
      std::shared_ptr<llvm::Type*> llvm_type;
      bool selfReferencing;
      size_t uid = 0;
    private:
      std::string name;
      SubSemanticNode m_node;
  };

}

namespace Semantic {
  // TODO: should this be implemented in terms of operator == ?
  bool compareTypes(Parsing::SemanticDeclarationNode s1, Parsing::SemanticDeclarationNode s2);
}

#endif
