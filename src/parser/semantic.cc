#include "semantic.h"
#include "ast.h"
#include "../utils/debug.h"

using namespace Parsing;

SemanticTree::SemanticTree() {
  counter = 0;
  loopDepth = 0;
  currentPos = 0;
  nodes.push_back(make_shared<SemanticNode>(-1));
  counter++;
}

void SemanticTree::addChild(Pos pos, string name) {
  nodes.push_back(make_shared<SemanticNode>(currentPos));


  // save the struct definitions
  if (name != "@@") {

    if(structMap.find(name) == structMap.end()) {
      stack<int> st = stack<int>();
      st.push(counter);
      structMap[name] = st;
    } else {

      SubSemanticNode helpNode;

      // delete not active nodes
      while(!structMap[name].empty()) {
        int id = structMap[name].top();
        int parent = nodes[id]->getParentIndex();
        if (nodes[parent]->isActive()) {
          helpNode = nodes[id];
          break;
        } else {
          structMap[name].pop();
        }
      }

      // add it to the Map
      if (structMap[name].empty() || nodes[structMap[name].top()]->getParentIndex() != currentPos) {
          structMap[name].push(counter);
      } else {
          throw Parsing::ParsingException("no redefinition of " + name, pos);
      }
    }
  }

  counter++;
  currentPos = counter - 1;
}

void SemanticTree::goUp() {
  nodes[currentPos]->disable();
  currentPos = nodes[currentPos]->getParentIndex();
}

void SemanticTree::addGotoLabel(string str) {
  gotoLabels.push_back(str);
}

pair<bool, string> SemanticTree::checkGotoLabels() {
  for (auto str : gotoLabels) {
    if (labelMap.find(str) == labelMap.end()) {
      return make_pair(false, str);
    }
  }

  return make_pair(true, "");
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
}

void SemanticTree::unsetCurrentFunction() {
  m_currentFunction.reset();
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
  // TODO this will not work for structs right now
  return (a->toString()) == (b->toString());
}

SemanticDeclarationNode SemanticTree::createType(TypeNode typeNode, Pos pos) {
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

       while(!structMap[name].empty()) {
        int id = structMap[name].top();
       // cout<<"Id : "<<id<<endl;
        int parent = nodes[id]->getParentIndex();
        if (nodes[parent]->isActive()) {
          helpNode = nodes[id];
          break;
        } else {
          structMap[name].pop();
        }
       }

       if (helpNode) {
        myDeclaration = make_shared<StructDeclaration>("@" + type, helpNode);
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
    return createType(typeNode, pos);
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

SemanticDeclarationNode SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos) {

  if (declarator && typeNode) {
    string name = declarator->getIdentifier();
    string type = typeNode->toString();

    SemanticDeclarationNode ret;
    auto decl = helpConvert(typeNode, declarator, ret, pos);

#ifdef DEBUG
    cout<<" DECL : "<<name<<" : " <<decl->toString()<<endl;
#endif

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
      if (st->size() > 0 && st->top().first == currentPos) {
        if (currentPos !=0 || 
            !(
            hasSameType(st->top().second, decl)
            )) {
          throw Parsing::ParsingException("no redefinition of " + name, pos);
        }
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

    if (st->size() == 0) {
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

  
bool isScalarType(SemanticDeclarationNode) {return true;}
bool hasScalarType(SubExpression s) {
  return isScalarType(s->getType());
}
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
bool isIntegerType(SemanticDeclarationNode) {return true;}
bool hasIntegerType(SubExpression) {return true;}
bool isRealType(SemanticDeclarationNode) {return true;}
bool hasRealType(SubExpression) {return true;}
bool isNullPtrConstant(SubExpression s);

bool isNullPtrConstant(SubExpression s) {
  // TODO: this has to check if the constant is actually 0
  if (auto s_as_constant = dynamic_pointer_cast<Constant>(s)) {
    return true;
  }
  return false;
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
