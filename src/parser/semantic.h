#ifndef SEMANTIC_H
#define SEMANTIC_H

#include <iostream>
#include <map>
#include <vector>
#include <stack>
#include "../pos.h"
#include "parserException.h"
#include "astNode.h"
#include "typeNode.h"
#include "declaratorNode.H"

using namespace std;

// TODO change string to TypeNode
// conflict in C++ dependencies

namespace Parsing {

typedef stack<pair<int, pair<TypeNode, int> > > TypeStack;

class SemanticTree {

  class SemanticNode {
    public:
      SemanticNode(int parent) : parent(parent), active(true) { };

      void disable() {
        active = false;
      }

      int getParentIndex() {
        return parent;
      }

      bool isActive() {
        return active;
      }

    private:
      int parent;
      bool active;
  };

  private:
    vector<SemanticNode> nodes;
    int currentPos;
    int counter;
    map<string, TypeStack *> declarationMap;


  public:
    SemanticTree();

    void addChild();
    void goUp();
    void deleteNotActiveNodes(TypeStack *st);
    void addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos);
    pair<TypeNode, int> lookUpType(string name, Pos pos);
};

}
#endif
