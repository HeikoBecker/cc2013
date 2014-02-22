#ifndef SEMANTIC_H
#define SEMANTIC_H

#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <stack>
#include <memory>
#include "../utils/pos.h"
#include "astNode.h"
#include "typeNode.h"
#include "declaratorNode.h"
#include "expressionNode.h"
#include "semadecl.h"

namespace Parsing {

class SemanticException: public std::runtime_error {
  public:
    SemanticException(const std::string& what_arg) : std::runtime_error(what_arg) {};
};

class SemanticNode;
typedef std::shared_ptr<SemanticNode> SubSemanticNode;

typedef std::stack<std::pair<int, Parsing::SemanticDeclarationNode> > TypeStack;

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

    void addDeclaration(std::string s, Parsing::SemanticDeclarationNode node) {
      decl[s] = node;
    }

    // TODO: make this private and a friend of StructDeclaration?
    std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>> type() {
      return std::vector<std::pair<std::string, Parsing::SemanticDeclarationNode>>(
            decl.begin(), decl.end()
          );
    }

    Parsing::SemanticDeclarationNode getNode(std::string name) {
      if (decl.find(name) == decl.end()) {
        throw SemanticException(name + " not found");
      } else {
        return decl[name];
      }
    }

  private:
    int parent;
    bool active;
    std::map<std::string, Parsing::SemanticDeclarationNode> decl;
};

class SemanticTree;

class SemanticForest {
  public:
    static std::shared_ptr<SemanticTree> filename2SemanticTree(std::string filename);
};

class SemanticTree {
  friend class SemanticForest;

  private:
    SemanticTree();
    std::vector<std::shared_ptr<SemanticNode> > nodes;
    int currentPos;
    Parsing::SemanticDeclarationNode m_currentFunction;
    int counter;
    std::map<std::string, TypeStack *> declarationMap;
    std::map<std::string, std::stack<std::pair<int,bool> > > structMap;
    int loopDepth; // depth inside loop for checking break; continue;

    // map for goto 
    std::set<std::string> labelMap;
    std::vector<std::pair<std::string, Pos>> gotoLabels;

  public:
    ~SemanticTree(); // we hold pointers in the declarationMap which we need to free
    // returns true, if the label could be added
    bool addLabel(std::string label);
    void addChild(Pos pos, std::string name="@@", bool forward = false);
    void goUp();
    void deleteNotActiveNodes(TypeStack *st);
    Parsing::SemanticDeclarationNode addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos);
    void increaseLoopDepth();
    void decreaseLoopDepth();
    /*
     * set and unset the current function type
     * this is used when entering a function definition to check that the return
     * type matches
     */
    void setCurrentFunction(Parsing::SemanticDeclarationNode);
    void unsetCurrentFunction();
    inline Parsing::SemanticDeclarationNode currentFunction() {return m_currentFunction;};
    void addGotoLabel(std::string str, Pos pos);
    bool isInLoop();
    Parsing::SemanticDeclarationNode createType(TypeNode t, Pos pos);
    Parsing::SemanticDeclarationNode helpConvert(
  TypeNode typeNode, 
  SubDeclarator declarator, 
  Parsing::SemanticDeclarationNode ret, Pos pos);

    // tells whether a struct has already been declared
    bool hasStructDeclaration(std::string name);

    void checkGotoLabels();
    Parsing::SemanticDeclarationNode lookUpType(std::string name, Pos pos);
};

}

namespace Semantic {
  bool isScalarType(Parsing::SemanticDeclarationNode);
  bool hasScalarType(Parsing::SubExpression);
  bool isArithmeticType(Parsing::SemanticDeclarationNode);
  bool hasArithmeticType(Parsing::SubExpression);
  bool isIntegerType(Parsing::SemanticDeclarationNode);
  bool hasIntegerType(Parsing::SubExpression);
  bool isRealType(Parsing::SemanticDeclarationNode);
  bool hasRealType(Parsing::SubExpression);
  bool isNullPtrConstant(Parsing::SubExpression s);
  bool isFunctionType(Parsing::SemanticDeclarationNode);
  bool isObjectType(Parsing::SemanticDeclarationNode);
  bool hasObjectType(Parsing::SubExpression);
  Parsing::SemanticDeclarationNode promoteType(Parsing::SemanticDeclarationNode s);
  std::pair<Parsing::SemanticDeclarationNode, Parsing::SemanticDeclarationNode> 
  applyUsualConversions(Parsing::SemanticDeclarationNode,
                        Parsing::SemanticDeclarationNode);
}

#endif
