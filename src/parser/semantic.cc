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

void SemanticTree::addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos) {
  if (declarator && typeNode) {
    TypeStack *st;

    string name = declarator->getIdentifier();
    int pointerCounter = declarator->getCounter();
#ifdef DEBUG
    string type = typeNode->toString();

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
