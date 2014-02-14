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
