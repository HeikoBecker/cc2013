#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <typeinfo>
#include "../lexer/punctuatortype.h"
#include "pprinter.h"

/**
 * This macro is meant to simplify a later transition from virtual inheritance
 * to CRTP static inheritance
 */
#define ASTNODE(X) X : public AstNode
#define EXPRESSION(X) X : public Expression
#define TYPE(X) X : public Type
#define STATEMENT(X) X : public Statement
#define JUMPSTATEMENT(X) X: public JumpStatement
#define ITERATIONSTATEMENT(X) X: public IterationStatement
#define DIRECTDECLARATOR(X) X : public DirectDeclarator

namespace Parsing {

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(PrettyPrinter & pp) {
      pp.pprint(std::string("IMPLEMENTATION MISSING!\n"));
      pp.pprint(std::string(typeid(*this).name()));
      pp.pprint('\n');
    };
};

class ASTNODE(Expression) { };

typedef std::shared_ptr<AstNode> AstChild;
typedef std::shared_ptr<AstNode> AstRoot;
/* TODO: this shouldn't be so general */
typedef std::shared_ptr<AstNode> BlockItem;
typedef std::shared_ptr<Expression> SubExpression;

class EXPRESSION(BinaryExpression)
{
  public:
    BinaryExpression(SubExpression lhs,
                     SubExpression rhs,
                     PunctuatorType op);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression lhs;
    SubExpression rhs;
    PunctuatorType op;
};

class EXPRESSION(UnaryExpression)
{
  public:
   UnaryExpression(PunctuatorType op,
       SubExpression operand);
   void prettyPrint(PrettyPrinter & pp) override;
  private:
   SubExpression operand;
   PunctuatorType op;
};

class EXPRESSION(VariableUsage)
{
  public:
    VariableUsage(std::string name);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::string name;
};

class EXPRESSION(FunctionCall)
{
  public:
    FunctionCall(SubExpression funcName,
                 std::vector<SubExpression> arguments);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression funcName;
    std::vector<SubExpression> arguments;
};

class EXPRESSION(TernaryExpression)
{
  public:
    TernaryExpression(SubExpression condition,
                      SubExpression lhs, 
                      SubExpression rhs);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    SubExpression condition;
    SubExpression lhs;
    SubExpression rhs;
};

class ASTNODE(Type) {};

class TYPE(BasicType) {
  // this type includes int/char/void  
  public:
    BasicType(std::string str);
    void prettyPrint(PrettyPrinter & pp) override;
  
  private:
    enum ReturnType {
      VOID,
      INT,
      CHAR
    };

    ReturnType type;
};

class TYPE(StructType) {
  // TODO Add content
  public:
    StructType();
    StructType(std::string str);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::string name;
};

class ASTNODE(Statement) {};

typedef std::shared_ptr<Statement> SubStatement;

class STATEMENT(CompoundStatement) {
  public:
    // TODO add inner blocks here
    CompoundStatement(std::vector<BlockItem> subStatements);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::vector<BlockItem> subStatements;
};

class STATEMENT(ExpressionStatement) {
  public:
    ExpressionStatement() { };
    ExpressionStatement(SubExpression ex) :expression(ex) { };
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    SubExpression expression;
};

class ASTNODE(Pointer) {
  public:
    Pointer(int counter) : counter(counter) { } ;
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    int counter;
};

class STATEMENT(SelectionStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    SelectionStatement(SubExpression ex, SubStatement ifStatement);
    SelectionStatement(
      SubExpression ex, 
      SubStatement ifStatement, 
      SubStatement elseStatement
    );

  // TODO : get rid of hasElseStatement ?
  private:
    SubExpression expression;
    SubStatement ifStatement;
    SubStatement elseStatement;
    bool hasElseStatement;
};

class STATEMENT(JumpStatement) { };

class JUMPSTATEMENT(GotoStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    GotoStatement(std::string label) : label(label) {};

  private:
    std::string label;
};

class JUMPSTATEMENT(ContinueStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    ContinueStatement() { };
};

class JUMPSTATEMENT(BreakStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    BreakStatement() { };
};

class JUMPSTATEMENT(ReturnStatement) {
  public:
    void prettyPrint(PrettyPrinter & pp) override;
    ReturnStatement() { };
    ReturnStatement(SubExpression ex) :expression(ex) { };
  
  private:
    SubExpression expression;
};

// iteration statement i.e. while and for
enum IterationEnum {
  WHILE,
  DOWHILE
};
typedef std::shared_ptr<JumpStatement> SubJumpStatement;
typedef std::shared_ptr<CompoundStatement> SubCompoundStatement;
typedef std::shared_ptr<ExpressionStatement> SubExpressionStatement;
 
typedef std::shared_ptr<SelectionStatement> SubSelectionStatement;



class STATEMENT(IterationStatement) { 
  public:
    IterationStatement(SubExpression ex, SubStatement st, IterationEnum k): expression(ex), statement(st), kind(k) { };
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    SubExpression expression;
    SubStatement statement;
    IterationEnum kind;
};

class STATEMENT(LabeledStatement) {
  public:
    LabeledStatement(std::string str, SubStatement st) : name(str), statement(st) { };
    void prettyPrint(PrettyPrinter & pp) override;

  private:
    std::string name;
    SubStatement statement;
};

typedef std::shared_ptr<LabeledStatement> SubLabeledStatement;
typedef std::shared_ptr<IterationStatement> SubIterationStatement;


class ASTNODE(IdentifierList) {
  public:
    IdentifierList(std::vector<std::string > list) : nameList(list) { };
    void prettyPrint(PrettyPrinter & pp) override;

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

class ASTNODE(DirectDeclaratorHelp) {
  public:
    DirectDeclaratorHelp() { };
    // TODO : implement the lists
};

typedef std::shared_ptr<DirectDeclaratorHelp> SubDirectDeclartorHelp;

class ASTNODE(DirectDeclarator) { 
  public:
    virtual void prettyPrint(PrettyPrinter & pp) {
      AstNode::prettyPrint(pp);
      pp.pprint(std::string("Called prettyPrint of DirectDeclarator directly. Why?\n"));
    };
};

typedef std::shared_ptr<DirectDeclarator> SubDirectDeclarator;

class ASTNODE(Declarator) {
  public:
    Declarator(int cnt, SubDirectDeclarator ast) : pointerCounter(cnt), directDeclarator(ast) { };
    int fixCompileError() {return pointerCounter;};
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    int pointerCounter;
    SubDirectDeclarator directDeclarator;
};

typedef std::shared_ptr<Declarator> SubDeclarator;

class DIRECTDECLARATOR(IdentifierDirectDeclarator) { 
  public:
   
    IdentifierDirectDeclarator(std::string str, SubDirectDeclartorHelp h) : identifier(str), help(h) { } ;

    IdentifierDirectDeclarator(std::string str) : identifier(str) { } ;

    void prettyPrint(PrettyPrinter & pp) override;
  // TODO pretty Print
  // handle SubDirectDeclaratorHelp not given
  
  private:
    std::string identifier;
    SubDirectDeclartorHelp help;
};

class DIRECTDECLARATOR(DeclaratorDirectDeclarator) { 
  public:
    DeclaratorDirectDeclarator(SubDeclarator d, SubDirectDeclartorHelp h) :
      declarator(d), help(h) { } ;
    // TODO pretty Print
 DeclaratorDirectDeclarator(SubDeclarator d) :
      declarator(d){ } ;
    
  private:
    SubDeclarator declarator;
    SubDirectDeclartorHelp help;
};

typedef std::shared_ptr<Type> TypeNode;

class ASTNODE(Declaration) {
  public:
    Declaration(TypeNode t, SubDeclarator declarator);
    Declaration(TypeNode t);
  private:
    TypeNode type;
    SubDeclarator declarator;
};

typedef std::shared_ptr<Declaration> DeclarationNode;

class ASTNODE(ExternalDeclaration) {
  public:
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement);
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator);
    ExternalDeclaration(TypeNode type);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    TypeNode type;
    SubDeclarator declarator;
    SubCompoundStatement compoundStatement;
};

typedef std::shared_ptr<ExternalDeclaration> ExternalDeclarationNode;

class ASTNODE(TranslationUnit) {
  public:
    TranslationUnit(std::vector<ExternalDeclarationNode> externalDeclarations);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    std::vector<ExternalDeclarationNode> externalDeclarations;
};

typedef std::shared_ptr<TranslationUnit> TUNode;

class ASTNODE(Parameter) {
  public:
    Parameter(TypeNode type, SubDeclarator declarator);
    Parameter(TypeNode type);
    void prettyPrint(PrettyPrinter & pp) override;
  private:
    TypeNode type;
    SubDeclarator declarator;
};

typedef std::shared_ptr<Parameter> ParameterNode;

/*

class ASTNODE(Declarator) {
  public:
    Declarator(Pointer ptr, DirectDeclarator) : name(str), statement(st) { };
    void prettyPrint(PrettyPrinter & pp) override;


}
*/





}
#endif
