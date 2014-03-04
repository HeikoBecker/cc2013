#include "semantic.h"
#include "ast.h"
#include "../utils/debug.h"
#include "../utils/exception.h"
#include "../utils/datastructures.h"

# include <algorithm>

using namespace std;
using namespace Parsing;

namespace Semantic {
bool isValidType(const SemanticDeclarationNode & s) {
  switch (s->type()) {
    case Type::FUNCTION:
      {
      auto s_as_function = static_pointer_cast<FunctionDeclaration>(s);
      // TODO: remove line below, or do we also need to check for this?
      auto params = s_as_function->parameter();
      if (!std::all_of(params.cbegin(), params.cend(), isValidType)) {
        return false;
      }
      auto ret_type = s_as_function->returnType();
      //  Functions shall not have a return type of type array or function
      return (ret_type->type() == Type::FUNCTION ? false : isValidType(ret_type));
      break;
      }
    case Type::POINTER:
      return isValidType(std::static_pointer_cast<PointerDeclaration>(s)->pointee());
    case Type::STRUCT:
      {
      auto s_as_struct = std::static_pointer_cast<StructDeclaration>(s);
      auto members = s_as_struct->node();
      (void) members; // FIXME: argh, can't get a vector of all members!<F6>
      }
    default:
      return true;
  }
  return true;
}
}

/*===========Semantic Node=============*/

SemanticNode::SemanticNode(int parent, bool insideStruct, bool forward) :
  parent(parent), active(true), insideStruct(insideStruct), forward(forward) {};

void SemanticNode::disable() {
  active = false;
}

int SemanticNode::getParentIndex() {
  return parent;
}

bool SemanticNode::isActive() {
  return active;
}

bool SemanticNode::isForward() {
  return forward;
}

void SemanticNode::setNotForward() {
  forward = false;
}

void SemanticNode::addDeclaration(
    std::string s,
    Parsing::SemanticDeclarationNode node) {
  decl[s] = node;
}


std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>> SemanticNode::type() {
  return std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>>(
      decl.begin(), decl.end()
      );
}

bool SemanticNode::isInsideStruct() {
  return insideStruct;
}

Parsing::SemanticDeclarationNode SemanticNode::getNode(std::string name) {
  if (decl.find(name) == decl.end()) {
    throw SemanticException(name + " not found");
  } else {
    return decl[name];
  }
}

/*====SemanticTree======*/

bool SemanticTree::hasStructDeclaration(std::string name) {
  if (structMap.find(name) == structMap.end()) {
    return false;
  } else {
      // check whether it is empty

      // delete not active nodes
      while(!structMap[name].empty()) {
        int id = structMap[name].top().first;
        int parent = nodes[id]->getParentIndex();
        if (nodes[parent]->isActive()) {
          break;
        }else {
          structMap[name].pop();
        }
      }

      return !structMap[name].empty();
  }
}


SemanticTree::SemanticTree() {
  counter = 0;
  loopDepth = 0;
  currentPos = 0;
  nodes.push_back(make_shared<SemanticNode>(-1, false));
  counter++;
}

SemanticTree::~SemanticTree() {
  for (auto const & it : declarationMap) {
    delete it.second;
  }
  declarationMap.clear();
}

void SemanticTree::addChild(Pos pos, string name, bool forward) {

   debug(SEMANTIC) << name<<" "<<forward;

  bool insideStruct = nodes[currentPos]->isInsideStruct();
  // save the struct definitions
  if (name != "@@") {
    insideStruct = true;

    if(structMap.find(name) == structMap.end()) {
      stack<pair<int,bool> > st = stack<pair<int, bool> >();
      st.push(
      make_pair(counter, forward)
      );
      structMap[name] = st;
    } else {
      SubSemanticNode helpNode;

      // delete not active nodes
      while(!structMap[name].empty()) {
        int id = structMap[name].top().first;
        int parent = nodes[id]->getParentIndex();
        if (nodes[parent]->isActive()) {
          helpNode = nodes[id];
          break;
        } else {
          structMap[name].pop();
        }
      }

      // add it to the Map
      if (structMap[name].empty() || nodes[structMap[name].top().first]->getParentIndex() != currentPos) {

          structMap[name].push(
            make_pair(counter, forward) 
          );
      } else { // it was alredy declared
          // it is a forward declaration
             bool lastForward = structMap[name].top().second;

             // take the node from the forward declaration
             if (lastForward) {

              if (!forward) {
                // it is not a forward declaration anymore
                auto helpPos = structMap[name].top().first;
                // std::cout<<"go to pos : "<<helpPos<<endl;
                nodes[helpPos]->setNotForward();
              }

              currentPos = structMap[name].top().first;
              return;
             } else {
               // it is a redefinition
                throw Parsing::ParsingException("no redefinition of " + name, pos);
             }
      }
    }
  }
  nodes.push_back(make_shared<SemanticNode>(currentPos, insideStruct, forward));
  counter++;
  currentPos = counter - 1;
  }

void SemanticTree::goUp() {
  //debug(SEMANTIC)<<"go up";
  nodes[currentPos]->disable();
  currentPos = nodes[currentPos]->getParentIndex();
}

void SemanticTree::addGotoLabel(string str, Pos pos) {
  gotoLabels.push_back(make_pair(str, pos));
}

void SemanticTree::checkGotoLabels() {
  for (auto info : gotoLabels) {
    if (labelMap.find(info.first) == labelMap.end()) {
          throw Parsing::ParsingException("label " + info.first +" is not defined", info.second);
    }
  }
}

void SemanticTree::deleteNotActiveNodes(TypeStack *st) {
  while (st->size() > 0 && 
      !nodes[st->top().first]->isActive()
      ) {
    st->pop();
  }
}

bool SemanticTree::isInLoop() {
  return loopDepth > 0;
}

void SemanticTree::increaseLoopDepth() {
  loopDepth++;
}

void SemanticTree::decreaseLoopDepth() {
  loopDepth--;
}

void SemanticTree::setCurrentFunction(SemanticDeclarationNode s) {
  m_currentFunction = s;
  // delete label variables
  labelMap.clear();
  gotoLabels.clear();
}

void SemanticTree::unsetCurrentFunction() {
  m_currentFunction.reset();
  // check goto label here
  checkGotoLabels();
}

bool SemanticTree::addLabel(string label) {
  if (labelMap.find(label) == labelMap.end()) {
    labelMap.insert(label);
    return true;
  } 
  return false;
}

// checks whether two types declarations are the same 
bool hasSameType(SemanticDeclarationNode a, SemanticDeclarationNode b) {
  // TODO get rid of this method alltogether 
  return Semantic::compareTypes(a,b); 
}

SemanticDeclarationNode SemanticTree::createType(TypeNode typeNode, Pos pos) {
  static std::map<std::string,SemanticDeclarationNode> structname2struct {};
   SemanticDeclarationNode myDeclaration;

    string type = typeNode->toString();
     if (type == "int") {
       myDeclaration = make_shared<IntDeclaration>();
     } else if (type == "char") {
       myDeclaration = make_shared<CharDeclaration>();
     } else if (type == "void") {
       // TODO This might be an error, but does not need to be
       // depends on the contex
       myDeclaration = make_shared<VoidDeclaration>();
     } else {
       // TODO structs
       string name = "@" + type;
        
       SubSemanticNode helpNode;
       auto id = int{};
       while(!structMap[name].empty()) {
        id = structMap[name].top().first;
        int parent = nodes[id]->getParentIndex();
        if (nodes[parent]->isActive()) {
          helpNode = nodes[id];
          break;
        } else {
          structMap[name].pop();
        }
       }

       if (helpNode) {
         auto ptr = (size_t) helpNode.operator->(); // TODO FIXME WARNING HACK !!!!!
         auto name = "@" + type + std::to_string(ptr);
         if (structname2struct.find(name) == structname2struct.end()) {
           debug(SEMANTIC) << "New";
           myDeclaration = make_shared<StructDeclaration>(name, helpNode, nodes[id]->isActive());
           structname2struct[name] = myDeclaration;
         } else {
           debug(SEMANTIC) << "Old";
           myDeclaration = structname2struct[name];
           std::static_pointer_cast<StructDeclaration>(myDeclaration)->selfReferencing =  nodes[id]->isActive();
         }
       } else {
          throw Parsing::ParsingException("the struct @" + type + "is not defined", pos);
       }
     }
   return myDeclaration;
}

SemanticDeclarationNode SemanticTree::helpConvert(
  TypeNode typeNode, 
  SubDeclarator declarator, 
  SemanticDeclarationNode ret,
  Pos pos
  ) {


   SemanticDeclarationNode myDeclaration;


   if (!declarator) {
     auto s =  createType(typeNode, pos);
     if(s->type() == Semantic::Type::STRUCT) {
       auto s_as_struct = std::static_pointer_cast<StructDeclaration>(s);
       if(s_as_struct->isSelfReferencing()) {
         throw Parsing::ParsingException("the struct is referencing itself", pos);
       }
     }

     return s;
   }


   TypeNode t = typeNode;
   SubDeclarator decl = declarator;
   pair<int,bool> res = declarator->getPointers();

   if (ret) {
     myDeclaration = ret; 

     int pointerCounter = res.first;
    if (pointerCounter != 0) {
      myDeclaration =  make_shared<PointerDeclaration>(pointerCounter-1, myDeclaration);
    } 

   } else {

     int pointerCounter = res.first;

     myDeclaration = createType(typeNode, pos);

    if (pointerCounter != 0) {
      myDeclaration =  make_shared<PointerDeclaration>(pointerCounter-1, myDeclaration);
    }  else {

      auto s =  myDeclaration;
     if(s->type() == Semantic::Type::STRUCT) {
       auto s_as_struct = std::static_pointer_cast<StructDeclaration>(s);
       if(s_as_struct->isSelfReferencing()) {
         throw Parsing::ParsingException("the struct is referencing itself", pos);
       }
     }
   }
   }

   if (res.second) {


    auto h = declarator->getNextDeclarator();

    // TODO add parameter here
    //
    auto pa = declarator->getNextParameter();


    auto  par = std::vector<SemanticDeclarationNode>(); 

    for(auto p : pa) {
      SemanticDeclarationNode empty;
      par.push_back(helpConvert(p->getType(), p->getDeclarator(),empty, pos));
    }


    auto ret = make_shared<FunctionDeclaration>(myDeclaration, par);

    if (h) {
       return helpConvert(t, h, ret, pos);
    } else {
      return ret;
    }
   } 
     return myDeclaration;
}

SemanticDeclarationNode SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos, bool isForwardFunction) {

  SemanticDeclarationNode ret;
  if (declarator && typeNode) {
    string name = declarator->getIdentifier();
    if (name == "@NAMELESS") {
      return  ret;
    }
    string type = typeNode->toString();

    auto decl = helpConvert(typeNode, declarator, ret, pos);

    debug(SEMANTIC) <<" DECL : "<<name<<" : " <<decl->toString();
    // std::cout<<" DECL : "<<name<<" : " <<decl->toString()<<std::endl;
  
    // don't allow functions inside a struct 
    if (nodes[currentPos]->isInsideStruct()) {
      debug(SEMANTIC)  <<"current pos " <<currentPos;
      if(!Semantic::isObjectType(decl)) {
      throw ParsingException("A function definition is not allowed inside a struct ", pos);
      }
    }

    // don't allow struct S a; if S is forward declaration
    if (decl->type() == Semantic::Type::STRUCT) {
      auto structDecl = static_pointer_cast<StructDeclaration>(decl);
      if (structDecl->node()->isForward()) {
      throw ParsingException("The size of " + name + " cannot be determined", pos);
      }
    }

    if (declarator->hasMoreThanOneParameterList()) {
      throw ParsingException("The declaration of  " + name + " has more than one parameter list", pos);
    }


    if (!Semantic::isValidType(decl)) {
      throw ParsingException("Invalid type: " + decl->toString(), pos);
    }


    if (Semantic::isIncompleteType(decl)) {
        throw Parsing::ParsingException(decl->toString() + " has incomplete type", pos);
    }

    // add function to map
    if (decl->type() == Semantic::Type::FUNCTION) {
      if (functionMap.find(name) == functionMap.end()){
          functionMap[name] = make_pair(decl, isForwardFunction);
      } else {
        bool wasForward = functionMap[name].second;
        auto lastDecl = functionMap[name].first; 
        if (!hasSameType(lastDecl, decl)) {
          throw Parsing::ParsingException("the functions do not have the same type ", pos);
        }

        if (wasForward) {
          functionMap[name] = make_pair(decl, isForwardFunction);
        } else {
          if (!isForwardFunction) {
            throw Parsing::ParsingException("No redefinition of  " + name, pos);
          }
        }
      }

    }

    // This is the old code 
    TypeStack *st;

    if (name == "NONAME") {
      return decl;
    }

    if (declarationMap.find(name) == declarationMap.end()) {
      st = new stack<pair<int, SemanticDeclarationNode> >();
      st->push(make_pair(currentPos, decl ));
      nodes[currentPos]->addDeclaration(name, decl);
      declarationMap[name] = st;
    } else {
      st = declarationMap[name];

      deleteNotActiveNodes(st);

      // NO redefinitions
      if (!st->empty() && st->top().first == currentPos) {

        auto prevDecl = st->top().second;

        if (!hasSameType(prevDecl, decl)) {
          throw Parsing::ParsingException("the declaration of  " + name + " has not the same type", pos);
        }

        if (decl->type() !=  Semantic::Type::FUNCTION && currentPos != 0) {
          throw Parsing::ParsingException("no redefinition of " + name, pos);
        }
        decl = prevDecl;
      } 

      declarationMap[name]->push(make_pair(currentPos, decl ));
      nodes[currentPos]->addDeclaration(name, decl);
    }
    return decl;
  } else {
    // TODO should this throw an error ?
    return SemanticDeclarationNode{}; //FIXME: is this  the correct thing to return?
  }
}

SemanticDeclarationNode SemanticTree::lookUpType(string name, Pos pos) {
  if (declarationMap.find(name) != declarationMap.end()) {
    TypeStack *st =  declarationMap[name];

    deleteNotActiveNodes(st);
    // delete not active nodes

    if (st->empty()) {
      throw Parsing::ParsingException(name+ " is not declared in this scope", pos);
    } else {
      return st->top().second;
    }
  } else {
    throw Parsing::ParsingException(name+ " is not declared in this scope", pos);
  }
}

shared_ptr<SemanticTree> SemanticForest::filename2SemanticTree(std::string filename)
{
  static auto names2trees = map<std::string, shared_ptr<SemanticTree>>();
  if (names2trees.find(filename) == names2trees.end()) {
        auto tree = shared_ptr<SemanticTree>(new SemanticTree());
        names2trees[filename] = tree;
  }
  return names2trees[filename];
}

namespace Semantic {

  
bool isScalarType(SemanticDeclarationNode s) {
  // cherry picking relevant parts from C++'s type_traits as described at
  // http://www.cplusplus.com/reference/type_traits/
  // (just removing everything not in C4
  return (  isArithmeticType(s)
          || s->type() == Type::POINTER
          // function can be converted to function pointer
          || s->type() == Type::FUNCTION
          );
}
bool hasScalarType(SubExpression s) {
  return isScalarType(s->getType());
}

/*******************************************************************************
 * Integer and floating types are collectively called arithmetic types. 
 * Each arithmetic type belongs to one type domain: the real type domain
 * comprises the real types, lthe complex type domain comprises the complex types.
 * 
 * Note: We don't have complex types, therefore real type => arithmetic type
 *******************************************************************************/

bool isArithmeticType(SemanticDeclarationNode s) {
  /* As we don't support floading point types,
   * arithmetic types are limited to integral types.
   * Those are limited to char and int in our implementation
   */
  switch (s->type()) {
    case Type::INT:
    case Type::CHAR:
      return true;
    default:
      return false;
  }
}

bool hasArithmeticType(SubExpression s) {
  return isArithmeticType(s->getType());
}
bool isIntegerType(SemanticDeclarationNode s) {
/* Currently this is the same as isArithmeticType.
 * However, if we ever want to expand the compiler to a larger subset of C,
 * those methods need to be extended */
  switch (s->type()) {
    case Type::INT:
    case Type::CHAR:
      return true;
    default:
      return false;
  }
}
bool hasIntegerType(SubExpression s) {
  return isIntegerType(s->getType());
}

bool isRealType(SemanticDeclarationNode s) {
  return isArithmeticType(s);
}
bool hasRealType(SubExpression s) {
  return isRealType(s->getType());
}
bool isNullPtrConstant(SubExpression s);

bool isNullPtrConstant(SubExpression s) {
  if (auto s_as_constant = dynamic_pointer_cast<Constant>(s)) {
    if (dynamic_pointer_cast<NullDeclaration>(s_as_constant->getType())) {
      return true;
    }
  }
  return false;
}

bool isFunctionType(SemanticDeclarationNode s) {
  return s->type() == Type::FUNCTION;
}

bool isIncompleteType(SemanticDeclarationNode s) {
  switch (s->type()) {
    case Type::STRUCT:
      {
        auto s_as_struct = std::static_pointer_cast<StructDeclaration>(s);
        return s_as_struct->node()->isForward();
      }
    case Type::VOID:
      return true;
    default:
      return false;
  }
}

bool isObjectType(SemanticDeclarationNode s) {
  return !isFunctionType(s);
}

bool hasObjectType(SubExpression s) {
  return isObjectType(s->getType());
}

bool isCompleteObjectType(SemanticDeclarationNode s) {
  return !(isFunctionType(s) || isIncompleteType(s));
}

bool hasCompleteObjectType(SubExpression s) {
  return isObjectType(s->getType());
}

SemanticDeclarationNode promoteType(SemanticDeclarationNode s) {
  // C defines some promotions
  // the only one we currently care about is from char to int
  // FIXME: note that this function doesn't really do type promotion
  // for this it would need both types
  // and more semantic knowledge
  if (dynamic_pointer_cast<CharDeclaration>(s)) {
    return make_shared<IntDeclaration>();
  }
  return s;
}

  std::pair<SemanticDeclarationNode, SemanticDeclarationNode> applyUsualConversions(SemanticDeclarationNode s1,SemanticDeclarationNode s2)
{
  //6.3.1.8 Usual arithmetic conversions
  // many rules which are not relevant, because we have no unsigned and only int
  // + char
  if (compareTypes(s1, s2)) {
    return make_pair<>(s1,s2);
  }
  // else
  // If both operands have the same type, then no further conversion is needed.
  //Otherwise, if both operands have signed integer types or both have unsigned
  //integer types, the operand with the type of lesser integer conversion rank is
  //converted to the type of the operand with greater rank.
  // ==> in our case: char -> int
  auto s1_type = s1->type();
  auto s2_type = s2->type();
  if (s1_type == Type::INT && s2_type == Type::CHAR) {
    auto int_type = make_shared<IntDeclaration>();
    return make_pair<>(s1, int_type);
  }
  if (s2_type == Type::INT && s1_type == Type::CHAR) {
    auto int_type = make_shared<IntDeclaration>();
    return make_pair<>(int_type, s2);
  }
  // the other rules don't apply in our limited subset
  // TODO: throw an exception in that case?
  return make_pair<>(s1,s2);
}

}
