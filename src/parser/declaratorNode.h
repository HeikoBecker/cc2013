#ifndef PARSER_DECLARATOR_H
#define PARSER_DECLARATOR_H
#pragma once

#include "astNode.h"
#include "typeNode.h"
#include <utility> // for pair
#include <string>


namespace Parsing {

class ASTNODE(IdentifierList) {
  public:
    IdentifierList(std::vector<std::string > list, Pos pos);
    PPRINTABLE

  private:
    std::vector<std::string> nameList;
};

typedef std::shared_ptr<IdentifierList> SubIdentifierList;


enum DirectDeclaratorHelpEnum {
  PARAMETERLIST,
  IDENTIFIERLIST,
  EMPTYLIST,
  EPSILON
};

/* required forward declaration + typedef forward declaration for
 * DirectDeclaratorHelp*/

class Declarator;
typedef std::shared_ptr<Declarator> SubDeclarator;



class Parameter;
typedef std::shared_ptr<Parameter> ParameterNode;

/* TODO: If we want to use only one class for DirectDeclaratorHelp
   * we would need to implement something like Boost::Variant
   * to efficiently store the members
   * or at least some clever use of placement new
   *
   * For now, we will just go with wasting space
   *
   *
   * TODO:
   * Furthermore it should be unnecessary to ahve a DirectDeclaratorHelper,
   * which has a DirectDeclaratorHelper as a child
   * This sounds like something that should be solved with a list/vector
   */
  class ASTNODE(DirectDeclaratorHelp) {
    public:
      DirectDeclaratorHelp(Pos pos);
      DirectDeclaratorHelp(std::vector<ParameterNode> paramList, Pos pos);
      DirectDeclaratorHelp(SubIdentifierList idList, Pos pos);
      std::vector<ParameterNode> getParameter() { return paramList; }
      bool canBeFunction();
      bool containsOnlyOneVoidIfSpecified();
       
      PPRINTABLE
    private:
        DirectDeclaratorHelpEnum helperType;
        // those are mutually exclusive:
        std::vector<ParameterNode> paramList;
        SubIdentifierList idList;
  };

  typedef std::shared_ptr<DirectDeclaratorHelp> SubDirectDeclaratorHelp;
  class DirectDeclarator;

  typedef std::shared_ptr<DirectDeclarator> SubDirectDeclarator;

  class ASTNODE(DirectDeclarator) { 
    CONS_INTER(DirectDeclarator)
    public:
      PPRINTABLE
      virtual std::string getIdentifier() {
        return "NONAME";
      }
      virtual std::vector<ParameterNode> getParameter() { return std::vector<ParameterNode>(); }


      virtual std::vector<ParameterNode> getNextParameter() { return std::vector<ParameterNode>(); }
      virtual bool canBeFunctionDefinition() {
        return false;
      }
      virtual bool hasName() {
        return false;
      }

      virtual bool isFunction() {
        return false;
      }

      virtual std::pair<int,bool>  getPointers() {
        return std::make_pair(0, false);
      }

      virtual SubDeclarator getSubDeclarator() {
        // return empty 
        throw "Exception";
        SubDeclarator s;
        return s;
      }

      virtual SubDeclarator getNextDeclarator() {
        // return empty 
        throw "exception";
        SubDeclarator s;
        return s;
      }

  };


   class ASTNODE(Declarator) {
    public:
      Declarator(int cnt, SubDirectDeclarator ast, Pos pos);
      std::string getIdentifier() { return (directDeclarator ? directDeclarator->getIdentifier() : "FIX THIS!"); }
      int getCounter() { return pointerCounter; }
      std::vector<ParameterNode> getParameter() { 
        return directDeclarator->getParameter(); 
      }


      std::vector<ParameterNode> getNextParameter() { 
        return directDeclarator->getNextParameter(); 
      }

      bool canBeFunctionDefinition() { return directDeclarator->canBeFunctionDefinition();}
      bool hasName() {
        if(directDeclarator) {
          return directDeclarator->hasName();
        }

        return false;
      }

      std::pair<int,bool> getPointers() {
        if(directDeclarator) {
          auto p =  directDeclarator->getPointers();
          p.first = p.first + pointerCounter;
          return p;
        }

        return std::make_pair(pointerCounter, false);
      }

      SubDeclarator getNextDeclarator() {
        // return empty 
        if (directDeclarator) {
          return directDeclarator->getNextDeclarator();
        }
        return empty;
      }

      SubDeclarator getSubDeclarator() {
        // return empty 
        if (directDeclarator) {
          return directDeclarator->getSubDeclarator();
        }
        return empty;
      }

      // get SubDeclarator
      PPRINTABLE
    private:
        // always empty
        SubDeclarator empty;
        int pointerCounter;
        SubDirectDeclarator directDeclarator;
  };


  class DIRECTDECLARATOR(IdentifierDirectDeclarator) { 
    public:

      IdentifierDirectDeclarator(std::string str,
          std::vector<SubDirectDeclaratorHelp> h,
          Pos pos);

      IdentifierDirectDeclarator(std::string str, Pos pos);
      virtual std::string getIdentifier() {
        return identifier;
      }

      std::pair<int, bool> getPointers() {
        // we are finished in our goal
        return std::make_pair(0, help.size() != 0);
      }


      virtual std::vector<ParameterNode> getNextParameter() { 
        if (help.size() == 0) {
          // this should never happen
          return std::vector<ParameterNode>(); 
        } else {
          return help[0]->getParameter();
        }
      }

      virtual std::vector<ParameterNode> getParameter() { 
        if (help.size() == 0) {
          return std::vector<ParameterNode>(); 
        } else {
          return help[0]->getParameter();
        }
      }

      bool hasName() {
        return true;
      }

      bool canBeFunctionDefinition() {
        return help.size() == 1 && help[0]->canBeFunction() && help[0]->containsOnlyOneVoidIfSpecified();
      }

      SubDeclarator getSubDeclarator() {
        // return empty 
        return s;
      }

      SubDeclarator getNextDeclarator() {
        // return empty 
        return s;
      }




      bool isFunction() {
        return help.size() !=0;
      }

      PPRINTABLE

    private:
        SubDeclarator s;
        std::string identifier;
        std::vector<SubDirectDeclaratorHelp> help;
  };

  class DIRECTDECLARATOR(DeclaratorDirectDeclarator) { 
    public:
      DeclaratorDirectDeclarator(SubDeclarator d,
          std::vector<SubDirectDeclaratorHelp> h,
          Pos pos);
      DeclaratorDirectDeclarator(SubDeclarator d, Pos pos); 
      PPRINTABLE
      virtual std::string getIdentifier() {
        return declarator->getIdentifier();
      }

      std::pair<int, bool> getPointers() {

        if (help.size() == 0 ) {
          return declarator->getPointers();
        }

        return std::make_pair(0, true);
      }

      virtual bool canBeFunctionDefinition() {
        if (!declarator) {
          return help.size() == 1 && help[0]->canBeFunction() && help[0]->containsOnlyOneVoidIfSpecified();
        } else {
          return help.size() <= 1 && declarator->canBeFunctionDefinition() && (help.size()==0 ? true : help[0]->containsOnlyOneVoidIfSpecified());
        }
      }

      virtual std::vector<ParameterNode> getNextParameter() { 
        if (help.size() == 0) {
          // this should never happen
          return declarator->getNextParameter();
        } else {
          return help[0]->getParameter();
        }
      }
      virtual std::vector<ParameterNode> getParameter() { 
        if (help.size() == 0) {
          return std::vector<ParameterNode>(); 
        } else {
          return help[0]->getParameter();
        }
      }
      bool hasName() {
        // TODO Check whether this is working
        if (declarator) {
          return declarator->hasName();
        } 
        return false;
      }

      virtual SubDeclarator getSubDeclarator() {
        return declarator;
      }

      SubDeclarator getNextDeclarator() {
        if(help.size() == 0) {
          return declarator->getSubDeclarator();
        } else {
          return declarator;
        }
      }

    private:
      SubDeclarator declarator;
      std::vector<SubDirectDeclaratorHelp> help;
  };


class ASTNODE(Parameter) {
  public:
    Parameter(TypeNode type, SubDeclarator declarator, Pos pos);
    Parameter(TypeNode type, Pos pos);
    TypeNode getType() { return type; }
    SubDeclarator getDeclarator() { return declarator; }
    bool hasDeclarator();
    bool hasName();
    bool isVoid(); // tests whether there is only (void) and not (int, void) or something
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
};

}

#endif
