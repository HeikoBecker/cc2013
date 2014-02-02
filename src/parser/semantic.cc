#include "semantic.h"
#include "../utils/debug.h"

namespace Parsing {

SemanticTree::SemanticTree() {
  counter = 0;
  loopDepth = 0;
  currentPos = 0;
  nodes.push_back(SemanticNode(-1));
  counter++;
}

void SemanticTree::addChild() {
  nodes.push_back(SemanticNode(currentPos));
  counter++;
  currentPos = counter - 1;
}

void SemanticTree::goUp() {
  nodes[currentPos].disable();
  currentPos = nodes[currentPos].getParentIndex();
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
      !nodes[st->top().first].isActive()
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


SemanticDeclarationNode convertHelp(TypeNode typeNode, SubDeclarator declarator) {
  
  int pointerCounter = 0;

  if (declarator) {
    pointerCounter = declarator->getCounter();
  }

   SemanticDeclarationNode myDeclaration;

   string type;
   // it is a basic type
   type = typeNode->toString();
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
     myDeclaration = make_shared<StructDeclaration>();
   }

   if (pointerCounter != 0) {
     myDeclaration =  make_shared<PointerDeclaration>(pointerCounter-1, myDeclaration);
   } 

   return myDeclaration;
}


SemanticDeclarationNode convert(
  SemanticDeclarationNode myDeclaration, 
  SubDeclarator declarator) {

   // cout<<"TYPE : " << myDeclaration->toString()<<endl;

   if (declarator && declarator->canBeFunctionDefinition()) {
     // it is a function
     // get the parameter
     vector<ParameterNode> parameter = declarator->getParameter();
     vector<SemanticDeclarationNode> nodes;

     // cout <<"Number of paramters : "<< parameter.size()<<endl;

     for (ParameterNode par : parameter) {
       TypeNode type = par->getType();
       SubDeclarator decl = par->getDeclarator();

       nodes.push_back(
        convert( convertHelp(type,decl), decl ));
     }

     auto decl = declarator->getSubDeclarator();


     if (decl) {
       // cout<<"Declator is NOT empty !"<<endl;

      SemanticDeclarationNode returnType = make_shared<FunctionDeclaration>(myDeclaration, nodes);

      TypeNode tHelp;
      SemanticDeclarationNode innerFun = convert(returnType, decl);

      // TODO test that this is a function

      return innerFun;
      // TODO here we still have something to do
     } else {
      // cout<<" Declarator is empty " <<endl;
      return make_shared<FunctionDeclaration>(myDeclaration, nodes);
     }


   } else {
    return myDeclaration;
   }
}


void SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos) {

  if (declarator && typeNode) {
    string name = declarator->getIdentifier();
    int pointerCounter = declarator->getCounter();
    string type = typeNode->toString();

    SemanticDeclarationNode decl = convert( convertHelp(typeNode, declarator), declarator);
#ifdef DEBUG
    cout<<" DECL : "<<decl->toString()<<endl;
#endif

    // This is the old code 
    TypeStack *st;

#ifdef DEBUG

    for(int n=0; n<pointerCounter; n++) { type+="*"; };
    debug(PARSER) <<" SEMANTIC ADD : NUMBER: "<<currentPos<<" IDENTIFIER: "<<name<<" TYPE:"<<type ;
#endif
    if (name == "NONAME") {
      return ;
    }

    if (declarationMap.find(name) == declarationMap.end()) {
      st = new stack<pair<int, pair<TypeNode, int> > >();
      st->push(make_pair(currentPos, make_pair(typeNode, pointerCounter) ));
      declarationMap[name] = st;
    } else {
      st = declarationMap[name];

      deleteNotActiveNodes(st);

      // NO redefinitions
      if (st->size() > 0 && st->top().first == currentPos) {
        if (currentPos !=0 || 
            !(st->top().second.first->toString() == typeNode->toString() && st->top().second.second == pointerCounter)) {
          throw Parsing::ParsingException("no redefinition of " + name, pos);
        }
      } 

      declarationMap[name]->push(make_pair(currentPos, make_pair(typeNode, pointerCounter)));
    }
  } else {
    // TODO should this throw an error
  }
}

pair<TypeNode, int> SemanticTree::lookUpType(string name, Pos pos) {
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

}
