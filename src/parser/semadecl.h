#ifndef PARSER_SEMADECL_H
#define PARSER_SEMADECL_H

#include <string>
#include <memory>
#include <vector>

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
      virtual std::string toString() {
        return "SemanticDeclaration";
      }
      virtual Semantic::Type type() {throw;};
      bool isVoid() {
        return type() == Semantic::Type::VOID;
      }
  };

  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;

  class IntDeclaration : public SemanticDeclaration {
    public: 
      Semantic::Type type() override {return Semantic::Type::INT;}
      virtual std::string toString() {
        return "int";
      }
  };

  // only used to distinguish null pointer value from integer
  class NullDeclaration : public IntDeclaration {
      virtual std::string toString() {
        return "NULL";
      }
  };

  class CharDeclaration : public SemanticDeclaration {
    public :
      Semantic::Type type() override {return Semantic::Type::CHAR;}
      virtual std::string toString() {
        return "char";
      }
  };

  // void is not allowed, but void**
  class VoidDeclaration : public SemanticDeclaration {
    Semantic::Type type() override {return Semantic::Type::VOID;}
    public: 
      virtual std::string toString() {
        return "void";
      }
  };

  class PointerDeclaration : public SemanticDeclaration {
    public:
      // type is int, char, or void
      PointerDeclaration(int pointerCounter, Parsing::SemanticDeclarationNode type);
      Semantic::Type type() override {return Semantic::Type::POINTER;}
      Parsing::SemanticDeclarationNode pointee() {return child;};

      virtual std::string toString() {
        return "*" + child->toString();
      }

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
      Semantic::Type type() override {return Semantic::Type::POINTER;}
      const size_t size;
  };

  class FunctionDeclaration : public SemanticDeclaration {

    public:
      FunctionDeclaration(Parsing::SemanticDeclarationNode ret, std::vector<SemanticDeclarationNode> par); 
      Semantic::Type type() override {return Semantic::Type::FUNCTION;}

      std::vector<Parsing::SemanticDeclarationNode> parameter() {return m_parameter;};
      Parsing::SemanticDeclarationNode returnType() {return returnChild;};

      virtual std::string toString();

    private:
      Parsing::SemanticDeclarationNode returnChild;
      std::vector<Parsing::SemanticDeclarationNode> m_parameter;

  };

  class StructDeclaration : public SemanticDeclaration {

    public:  
      // name e.g. @S
      StructDeclaration(std::string n, SubSemanticNode s, bool forward, bool selfReferencing);
      Semantic::Type type() override {return Semantic::Type::STRUCT;}

      std::string toString() {
        return name;
      }

      bool isForward() {
        return forward;
      }

      bool isSelfReferencing() {
        return selfReferencing;
      }
      SubSemanticNode node() {return m_node;}
    private:
      std::string name;
      SubSemanticNode m_node;
      bool forward;
      bool selfReferencing;
  };

}

namespace Semantic {
  // TODO: should this be implemented in terms of operator == ?
  bool compareTypes(Parsing::SemanticDeclarationNode s1, Parsing::SemanticDeclarationNode s2);
}

#endif
