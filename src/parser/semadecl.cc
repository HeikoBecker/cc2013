#include "semadecl.h"
#include "semantic.h"
#include <string>

using namespace Parsing;


std::string SemanticDeclaration::toString()
{
  return "SemanticDeclaration";
}

Semantic::Type SemanticDeclaration::type() {
  throw;
}

bool SemanticDeclaration::isVoid() 
{
  return type() == Semantic::Type::VOID;
}



Semantic::Type IntDeclaration::type()
{
  return Semantic::Type::INT;
}

std::string IntDeclaration::toString()
{
  return "int";
}


std::string NullDeclaration::toString() {
  return "NULL";
}


Semantic::Type CharDeclaration::type() 
{
  return Semantic::Type::CHAR;
}

std::string CharDeclaration::toString() 
{
  return "char";
}



Semantic::Type VoidDeclaration::type() 
{
  return Semantic::Type::VOID;
}

std::string VoidDeclaration::toString()
{
  return "void";
}


StructDeclaration::StructDeclaration(std::string n, SubSemanticNode s, bool selfReferencing) : llvm_type(nullptr), selfReferencing(selfReferencing), name(n), m_node(s) {}

std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>> StructDeclaration::members()
{
  return this->m_node->type();
}


Semantic::Type StructDeclaration::type()
{
  return Semantic::Type::STRUCT;
}

std::string StructDeclaration::toString()
{
  return name;
}

bool StructDeclaration::isSelfReferencing()
{
  return selfReferencing;
}

SubSemanticNode StructDeclaration::node()
{
  return m_node;
}



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


Semantic::Type FunctionDeclaration::type()
{
  return Semantic::Type::FUNCTION;
}

std::vector<Parsing::SemanticDeclarationNode> FunctionDeclaration::parameter() 
{
  return m_parameter;
}


Parsing::SemanticDeclarationNode FunctionDeclaration::returnType()
{
  return returnChild;
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
    for (auto it = m_parameter.begin(); it != m_parameter.end(); ++it) {
      if ((*it)->type() == Semantic::Type::FUNCTION) {
        *it = std::make_shared<PointerDeclaration>(0, *it);
      }
    }
  }
} 

void FunctionDeclaration::rebindParameters(std::vector<Parsing::SemanticDeclarationNode> parameters)
{
  if (!(parameters.size() == 1 && parameters[0]->type() == Semantic::Type::VOID)) {
    m_parameter.clear();
    for (auto p: parameters) {
      m_parameter.push_back(p);
    }
    for (auto it = m_parameter.begin(); it != m_parameter.end(); ++it) {
      if ((*it)->type() == Semantic::Type::FUNCTION) {
        *it = std::make_shared<PointerDeclaration>(0, *it);
      }
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


Semantic::Type PointerDeclaration::type()
{
  return Semantic::Type::POINTER;
}

Parsing::SemanticDeclarationNode PointerDeclaration::pointee()
{
  return child;
}

std::string PointerDeclaration::toString()
{
  return "*" + child->toString();
}

ArrayDeclaration::ArrayDeclaration(Parsing::SemanticDeclarationNode type, size_t size)
  : PointerDeclaration(0, type), size(size) {}


Semantic::Type ArrayDeclaration::type()
{
  return Semantic::Type::POINTER;
}

std::string ArrayDeclaration::toString()
{
  return pointee()->toString() + "[" + std::to_string(size) + "]";
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
                         }
    case Type::POINTER: {
      auto ptype1 = std::static_pointer_cast<PointerDeclaration>(s1);
      auto ptype2 = std::static_pointer_cast<PointerDeclaration>(s2);
      return compareTypes(ptype1->pointee(), ptype2->pointee());
                        }
    case Type::STRUCT: {
      auto struct1 = std::static_pointer_cast<StructDeclaration>(s1);
      auto struct2 = std::static_pointer_cast<StructDeclaration>(s2);
      if (struct1->toString() != struct2->toString()) {
        return false;
      }
      return true;
                       }
    default:
      return false;
  }
}
