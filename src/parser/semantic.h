#ifndef SEMANTIC_H
#define SEMANTIC_H

#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <stack>
#include "../utils/pos.h"
#include "astNode.h"
#include "typeNode.h"
#include "declaratorNode.h"
#include "expressionNode.h"
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

    // TODO: make this private and a friend of StructDeclaration?
    std::vector<std::pair<std::string, SemanticDeclarationNode>> type() {
      return std::vector<std::pair<std::string, SemanticDeclarationNode>>(
            decl.begin(), decl.end()
          );
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
    SemanticDeclarationNode m_currentFunction;
    int counter;
    map<string, TypeStack *> declarationMap;
    map<string, stack<pair<int,bool> > > structMap;
    int loopDepth; // depth inside loop for checking break; continue;

    // map for goto 
    set<string> labelMap;
    vector<string> gotoLabels;

  public:
    // returns true, if the label could be added
    bool addLabel(string label);
    void addChild(Pos pos, string name="@@", bool forward = false);
    void goUp();
    void deleteNotActiveNodes(TypeStack *st);
    SemanticDeclarationNode addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos);
    void increaseLoopDepth();
    void decreaseLoopDepth();
    /*
     * set and unset the current function type
     * this is used when entering a function definition to check that the return
     * type matches
     */
    void setCurrentFunction(SemanticDeclarationNode);
    void unsetCurrentFunction();
    inline SemanticDeclarationNode currentFunction() {return m_currentFunction;};
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
  bool isScalarType(SemanticDeclarationNode);
  bool hasScalarType(SubExpression);
  bool isArithmeticType(SemanticDeclarationNode);
  bool hasArithmeticType(SubExpression);
  bool isIntegerType(SemanticDeclarationNode);
  bool hasIntegerType(SubExpression);
  bool isRealType(SemanticDeclarationNode);
  bool hasRealType(SubExpression);
  bool isNullPtrConstant(SubExpression s);
  bool isObjectType(SemanticDeclarationNode);
  bool hasObjectType(SubExpression);
  SemanticDeclarationNode promoteType(SemanticDeclarationNode s);
  std::pair<SemanticDeclarationNode, SemanticDeclarationNode> applyUsualConversions(SemanticDeclarationNode,SemanticDeclarationNode);
}

#endif
