#ifndef PARSER_DECLARATOR_H
#define PARSER_DECLARATOR_H
#pragma once

#include "astNode.h"
#include "expressionNode.h"
#include "statementNode.h"
#include "typeNode.h"

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
      PPRINTABLE
    private:
        DirectDeclaratorHelpEnum helperType;
        // those are mutually exclusive:
        std::vector<ParameterNode> paramList;
        SubIdentifierList idList;
  };

  typedef std::shared_ptr<DirectDeclaratorHelp> SubDirectDeclaratorHelp;

  class ASTNODE(DirectDeclarator) { 
    CONS_INTER(DirectDeclarator)
    public:
      virtual void prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel) {
        AstNode::prettyPrint(pp, indentLevel);
        pp.pprint(std::string("Called prettyPrint of DirectDeclarator directly. Why?\n"),
            indentLevel);
      };
      virtual std::string getIdentifier() {
        return "NONAME";
      }
  };

  typedef std::shared_ptr<DirectDeclarator> SubDirectDeclarator;

  class ASTNODE(Declarator) {
    public:
      Declarator(int cnt, SubDirectDeclarator ast, Pos pos);
      std::string getIdentifier() { return directDeclarator->getIdentifier(); }
      int getCounter() { return pointerCounter; }
      PPRINTABLE
    private:
        int pointerCounter;
        SubDirectDeclarator directDeclarator;
  };

  typedef std::shared_ptr<Declarator> SubDeclarator;

  class DIRECTDECLARATOR(IdentifierDirectDeclarator) { 
    public:

      IdentifierDirectDeclarator(std::string str,
          std::vector<SubDirectDeclaratorHelp> h,
          Pos pos);

      IdentifierDirectDeclarator(std::string str, Pos pos);
      virtual std::string getIdentifier() {
        return identifier;
      }

      PPRINTABLE

    private:
        std::string identifier;
        std::vector<SubDirectDeclaratorHelp> help;
  };

  class DIRECTDECLARATOR(DeclaratorDirectDeclarator) { 
    public:
      DeclaratorDirectDeclarator(SubDeclarator d,
          std::vector<SubDirectDeclaratorHelp> h,
          Pos pos);
      PPRINTABLE
        DeclaratorDirectDeclarator(SubDeclarator d, Pos pos); 
      virtual std::string getIdentifier() {
        return declarator->getIdentifier();
      }

    private:
      SubDeclarator declarator;
      std::vector<SubDirectDeclaratorHelp> help;
  };
}

#endif
