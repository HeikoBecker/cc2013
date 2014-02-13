#ifndef SEMANTIC_H
#define SEMANTIC_H

#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <stack>
#include "../utils/pos.h"
#include "parserException.h"
#include "astNode.h"
#include "typeNode.h"
#include "declaratorNode.h"
#include "semadecl.h"

using namespace std;

// conflict in C++ dependencies

namespace Parsing {

class SemanticException: public std::runtime_error {
  public:
    SemanticException(const std::string& what_arg) : std::runtime_error(what_arg) {};
};

class SemanticNode;
typedef shared_ptr<SemanticNode> SubSemanticNode;

typedef stack<pair<int, SemanticDeclarationNode> > TypeStack;

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

    void addDeclaration(string s, SemanticDeclarationNode node) {
      decl[s] = node;
    }

    SemanticDeclarationNode getNode(string name) {
      if (decl.find(name) == decl.end()) {
        throw SemanticException(name + " not found");
      } else {
        return decl[name];
      }
    }

  private:
    int parent;
    bool active;
    map<string, SemanticDeclarationNode> decl;
};

class SemanticTree;

class SemanticForest {
  public:
    static shared_ptr<SemanticTree> filename2SemanticTree(std::string filename);
};

class SemanticTree {
  friend class SemanticForest;

  private:
    SemanticTree();
    vector<shared_ptr<SemanticNode> > nodes;
    int currentPos;
    int counter;
    map<string, TypeStack *> declarationMap;
    map<string, stack<int> > structMap;
    int loopDepth; // depth inside loop for checking break; continue;

    // map for goto 
    set<string> labelMap;
    vector<string> gotoLabels;

  public:
    // returns true, if the label could be added
    bool addLabel(string label);
    void addChild(Pos pos, string name="@@");
    void goUp();
    void deleteNotActiveNodes(TypeStack *st);
    void addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos);
    void increaseLoopDepth();
    void decreaseLoopDepth();
    void addGotoLabel(string str);
    bool isInLoop();
    SemanticDeclarationNode createType(TypeNode t, Pos pos);
    SemanticDeclarationNode helpConvert(
  TypeNode typeNode, 
  SubDeclarator declarator, 
  SemanticDeclarationNode ret, Pos pos);

    pair<bool, string> checkGotoLabels();
    SemanticDeclarationNode lookUpType(string name, Pos pos);
};

}

namespace Semantic {
  using namespace Parsing;
  inline bool isScalarType(SemanticDeclarationNode) {return true;}
  inline bool hasScalarType(SubExpression) {return true;}
  inline bool isArithmeticType(SemanticDeclarationNode) {return true;}
  inline bool hasArithmeticType(SubExpression) {return true;}
  inline bool isIntegerType(SemanticDeclarationNode) {return true;}
  inline bool hasIntegerType(SubExpression) {return true;}
  inline bool isRealType(SemanticDeclarationNode) {return true;}
  inline bool hasRealType(SubExpression) {return true;}
  bool isNullPtrConstant(SubExpression s);
  SemanticDeclarationNode promoteType(SemanticDeclarationNode s);
}

#endif
