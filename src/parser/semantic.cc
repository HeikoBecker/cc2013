#include "semantic.h"

namespace Parsing {

SemanticTree::SemanticTree() {
  counter = 0;
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


void SemanticTree::deleteNotActiveNodes(TypeStack *st) {
  while (st->size() > 0 && 
      !nodes[st->top().first].isActive()
      ) {
    st->pop();
  }
}

void SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos) {
  TypeStack *st;

  string name = declarator->getIdentifier();
  int pointerCounter = declarator->getCounter();

#ifdef DEBUG
  string type = typeNode->toString();

  for(int n=0; n<pointerCounter; n++) { type+="*"; };
  cout<<" SEMANTIC ADD : NUMBER: "<<currentPos<<" IDENTIFIER: "<<name<<" TYPE:"<<type<<endl;
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
          !(st->top().second.first == typeNode && st->top().second.second == pointerCounter)) {
        throw Parsing::ParsingException("no redefinition of " + name, pos);
      }
    } 

    declarationMap[name]->push(make_pair(currentPos, make_pair(typeNode, pointerCounter)));
    
  }
}

pair<TypeNode, int> SemanticTree::lookUpType(string name, Pos pos) {
  if (declarationMap.find(name) != declarationMap.end()) {
    TypeStack *st =  declarationMap[name];

    deleteNotActiveNodes(st);
    // delete not active nodes

    if (st->size() == 0) {
      throw Parsing::ParsingException(name+ "is not declared in this scope", pos);
    } else {
      return st->top().second;
    }
  } else {
    throw Parsing::ParsingException(name+ "is not declared in this scope", pos);
  }
}

}
