#include "semantic.h"

SemanticTree::SemanticTree() {
  counter = 0;
  currentPos = 0;
  nodes.push_back(SemanticNode(-1));
  counter++;

}

void SemanticTree::addChild() {
  nodes.push_back(SemanticNode(currentPos));
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

void SemanticTree::addDeclaration(string name, TypeNode typeNode, Pos pos) {
  TypeStack *st;

  if (declarationMap.find(name) == declarationMap.end()) {
    st = new stack<pair<int, TypeNode> >();
    declarationMap[name] = st;
  } else {
    st = declarationMap[name];

    deleteNotActiveNodes(st);

    // NO redefinitions
    if (st->size() > 0 && st->top().first == currentPos) {
      throw Parsing::ParsingException("no redefinition of " + name, pos);
    } else {
      declarationMap[name]->push(make_pair(currentPos, typeNode));
    }
  }
}

TypeNode SemanticTree::lookUpType(string name, Pos pos) {
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
