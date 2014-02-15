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
      PointerDeclaration(int pointerCounter, SemanticDeclarationNode type);
      Semantic::Type type() override {return Semantic::Type::POINTER;}

      SemanticDeclarationNode pointee() {return child;};

      virtual std::string toString() {
        return "*" + child->toString();
      }

    private:
      SemanticDeclarationNode child;
  };

  class FunctionDeclaration : public SemanticDeclaration {

    public:
      FunctionDeclaration(SemanticDeclarationNode ret, std::vector<SemanticDeclarationNode> par); 
      Semantic::Type type() override {return Semantic::Type::FUNCTION;}

      std::vector<SemanticDeclarationNode> parameter() {return m_parameter;};
      SemanticDeclarationNode returnType() {return returnChild;};

      virtual std::string toString();

    private:
      SemanticDeclarationNode returnChild;
      std::vector<SemanticDeclarationNode> m_parameter;

  };

  class StructDeclaration : public SemanticDeclaration {

    public:  
      // name e.g. @S
      StructDeclaration(std::string n, SubSemanticNode s) {
        name = n;
        m_node = s;
      }
      Semantic::Type type() override {return Semantic::Type::STRUCT;}

      std::string toString() {
        return name;
      }
      SubSemanticNode node() {return m_node;}
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
