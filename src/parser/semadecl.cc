#include "semadecl.h"
#include <string>

using namespace Parsing;

std::string FunctionDeclaration::toString() {
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

FunctionDeclaration::FunctionDeclaration(SemanticDeclarationNode ret, std::vector<SemanticDeclarationNode> par) :returnChild(ret), m_parameter(par) 
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


PointerDeclaration::PointerDeclaration(int pointerCounter, SemanticDeclarationNode type) {
  if (pointerCounter == 0) {
    child = type;
  } else {
    child = std::make_shared<PointerDeclaration>(pointerCounter-1, type);
  }
}

using namespace Semantic;

bool Semantic::compareTypes(SemanticDeclarationNode s1, SemanticDeclarationNode s2) {
  if (s1->type() != s2->type()) {
    return false;
  }
  switch (s1->type()) {
    case Type::INT:
    case Type::CHAR:
    case Type::VOID:
      return true;
      break;
    case Type::FUNCTION: {
      auto ftype1 = std::static_pointer_cast<FunctionDeclaration>(s1);
      auto ftype2 = std::static_pointer_cast<FunctionDeclaration>(s2);
      auto same_return = compareTypes(ftype1->returnType(), ftype2->returnType());
      if (!same_return) {return false;}
      auto params1 = ftype1->parameter();
      auto params2 = ftype2->parameter();
      if (params1.size() != params2.size()) {return false;}
      // TODO: write zip function to avoid explicit  for loop
      for (signed int i = params1.size()-1; i >= 0; --i) { // without signed int >= 0 is always true
        if (!compareTypes(params1.at(i), params2.at(i))) {
          return false;
        }
      }
      return true;
      break;
                         }
    case Type::POINTER: {
      auto ptype1 = std::static_pointer_cast<PointerDeclaration>(s1);
      auto ptype2 = std::static_pointer_cast<PointerDeclaration>(s2);
      return compareTypes(ptype1->pointee(), ptype2->pointee());
      break;
                        }
    case Type::STRUCT: {
      auto stype1 = std::static_pointer_cast<StructDeclaration>(s1);
      auto stype2 = std::static_pointer_cast<StructDeclaration>(s2);
      return false;
      break;
                       }
    default:
      return false;
  }
}
