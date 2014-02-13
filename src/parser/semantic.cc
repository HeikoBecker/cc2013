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

void SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos) {

  if (declarator && typeNode) {
    string name = declarator->getIdentifier();
    string type = typeNode->toString();

    SemanticDeclarationNode ret;
    auto decl = helpConvert(typeNode, declarator, ret, pos);

#ifdef DEBUG
    cout<<" DECL : "<<decl->toString()<<endl;
#endif

    // This is the old code 
    TypeStack *st;

    if (name == "NONAME") {
      return ;
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
  } else {
    // TODO should this throw an error ?
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

namespace Semantic {
bool isNullPtrConstant(SubExpression s) {
  // TODO: this has to check if the constant is actually 0
  if (auto s_as_constant = dynamic_pointer_cast<Constant>(s)) {
    return true;
  }
  return false;
}
}
