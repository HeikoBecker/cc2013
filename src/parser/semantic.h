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

using namespace std;

// TODO change string to TypeNode
// conflict in C++ dependencies

namespace Parsing {

class SemanticDeclaration {
public: 
  virtual string toString() {
    return "SemanticDeclaration";
  }

};

typedef std::shared_ptr<SemanticDeclaration> SemanticDeclarationNode;

class IntDeclaration : public SemanticDeclaration {
public: 
 virtual string toString() {
    return "int";
  }
};

class CharDeclaration : public SemanticDeclaration {
public :
  virtual string toString() {
    return "string";
  }
};


// TODO : i am not sure
// void is not allowed, but void**
class VoidDeclaration : public SemanticDeclaration {
public: 
 virtual string toString() {
    return "void";
  }

};

class PointerDeclaration : public SemanticDeclaration {
  public:
   // type is int, char, or void
    PointerDeclaration(int pointerCounter, SemanticDeclarationNode type) {
      if (pointerCounter == 0) {
        child = type;
      } else {
        child = make_shared<PointerDeclaration>(pointerCounter-1, type);
      }
    }
  
  virtual string toString() {
    return child->toString()+"*";
  }
    
  private:
    SemanticDeclarationNode child;
};

class FunctionDeclaration : public SemanticDeclaration {

public:
  FunctionDeclaration(SemanticDeclarationNode ret, vector<SemanticDeclarationNode> par) :returnChild(ret), parameter(par) { }

  virtual string toString() {
    string str = "function (";
    bool first =true;
    for (SemanticDeclarationNode p : parameter) {
      if (!first) {
        str+= ",";
      }
      str += p->toString() ;

      first = false;
    }

    str +=") returning ";
    str += returnChild->toString();

    return str;
  }

private:
    SemanticDeclarationNode returnChild;
    vector<SemanticDeclarationNode> parameter;

};

class StructDeclaration : public SemanticDeclaration {
  // TODO : implement 
public:  
  virtual string toString() {
    return "struct ";
  }
};

typedef stack<pair<int, SemanticDeclarationNode> > TypeStack;

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

    int loopDepth; // depth inside loop for checking break; continue;

    // map for goto 
    set<string> labelMap;
    vector<string> gotoLabels;

  public:
    SemanticTree();
    // returns true, if the label could be added
    bool addLabel(string label);
    void addChild();
    void goUp();
    void deleteNotActiveNodes(TypeStack *st);
    void addDeclaration(TypeNode typeNode, SubDeclarator declarator, Pos pos);
    void increaseLoopDepth();
    void decreaseLoopDepth();
    void addGotoLabel(string str);
    bool isInLoop();
    pair<bool, string> checkGotoLabels();
    SemanticDeclarationNode lookUpType(string name, Pos pos);
};

}
#endif
