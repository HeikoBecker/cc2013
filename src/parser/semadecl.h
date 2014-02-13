#ifndef PARSER_SEMADECL_H
#define PARSER_SEMADECL_H

#include <string>
#include <memory>
#include <vector>

namespace Parsing {

  class SemanticNode;
  typedef std::shared_ptr<SemanticNode> SubSemanticNode;

  class SemanticDeclaration {
    public: 
      virtual std::string toString() {
        return "SemanticDeclaration";
      }
  };

  typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;

  class IntDeclaration : public SemanticDeclaration {
    public: 
      virtual std::string toString() {
        return "int";
      }
  };

  class CharDeclaration : public SemanticDeclaration {
    public :
      virtual std::string toString() {
        return "char";
      }
  };


  // void is not allowed, but void**
  class VoidDeclaration : public SemanticDeclaration {
    public: 
      virtual std::string toString() {
        return "void";
      }
  };

  class PointerDeclaration : public SemanticDeclaration {
    public:
      // type is int, char, or void
      PointerDeclaration(int pointerCounter, SemanticDeclarationNode type) {
        if (pointerCounter == 0) {
          child = type;
        } else {
          child = std::make_shared<PointerDeclaration>(pointerCounter-1, type);
        }
      }

      SemanticDeclarationNode pointee() {return child;};

      virtual std::string toString() {
        return "*" + child->toString();
      }

    private:
      SemanticDeclarationNode child;
  };

  class FunctionDeclaration : public SemanticDeclaration {

    public:
      FunctionDeclaration(SemanticDeclarationNode ret, std::vector<SemanticDeclarationNode> par) :returnChild(ret), m_parameter(par) 
    {
      if (m_parameter.size() == 1) {
        if (std::dynamic_pointer_cast<VoidDeclaration>(m_parameter.front())) {
          /* 6.7.6.3:
           * The special case of an unnamed parameter of type void as the only item in the list
           * specifies that the function has no parameters.
           */
          m_parameter.clear();
        }
      } 
    }

      std::vector<SemanticDeclarationNode> parameter() {return m_parameter;};
      SemanticDeclarationNode returnType() {return returnChild;};

      virtual std::string toString() {
        std::string str = "function (";
        bool first =true;
        for (SemanticDeclarationNode p : m_parameter) {
          if (!first) {
            str+= ",";
          }
          str += p->toString() ;

          first = false;
        }

        str +=") returning ";
        str += returnChild->toString();

        return str;
      }

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

      std::string toString() {
        return name;
      }
      SubSemanticNode node() {return m_node;}
    private:
      std::string name;
      SubSemanticNode m_node;
  };

}

#endif
