#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <memory>
#include <string>
#include <vector>
#include <typeinfo>
#include "../lexer/punctuatortype.h"
#include "../pos.h"
#include "pprinter.h"

/* This macro allows an easy switching of pprint in all methods*/
#define PPRINTABLE  void prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel = 0) override;

/* This macro is used in intermediate classes */
#define CONS_INTER(X) protected : X(Pos pos) : AstNode(pos){};

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
  protected:
    AstNode(Pos pos) : m_pos(std::move(pos)) {};
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(const PrettyPrinter & pp, unsigned int indentLevel) {
      pp.pprint(std::string("\nIMPLEMENTATION MISSING!\n"), indentLevel);
      pp.pprint(std::string(typeid(*this).name()), indentLevel);
      pp.pprint('\n', indentLevel);
    };
    Pos inline pos() {return m_pos;}
  private:
    Pos m_pos;
};

class ASTNODE(Expression) 
{
    CONS_INTER(Expression)

};

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
                     PunctuatorType op,
                     Pos pos);
    PPRINTABLE
  private:
    SubExpression lhs;
    SubExpression rhs;
    PunctuatorType op;
};

class EXPRESSION(UnaryExpression)
{
  public:
   UnaryExpression(PunctuatorType op,
       SubExpression operand,
       Pos pos);
   PPRINTABLE
  private:
   SubExpression operand;
   PunctuatorType op;
};

class EXPRESSION(VariableUsage)
{
  public:
    VariableUsage(std::string name, Pos pos);
    PPRINTABLE
  private:
    std::string name;
};

class EXPRESSION(FunctionCall)
{
  public:
    FunctionCall(SubExpression funcName,
                 std::vector<SubExpression> arguments,
                 Pos pos);
    PPRINTABLE
  private:
    SubExpression funcName;
    std::vector<SubExpression> arguments;
};

class EXPRESSION(TernaryExpression)
{
  public:
    TernaryExpression(SubExpression condition,
                      SubExpression lhs, 
                      SubExpression rhs,
                      Pos);
    PPRINTABLE
  private:
    SubExpression condition;
    SubExpression lhs;
    SubExpression rhs;
};

class ASTNODE(Type)
{
  CONS_INTER(Type)

  public:
    virtual bool canBeInFunctionDeclaration() { return true; };
};

class TYPE(BasicType) {
  // this type includes int/char/void  
  public:
    BasicType(std::string str, Pos pos);
    PPRINTABLE
  
  private:
    enum ReturnType {
      VOID,
      INT,
      CHAR
    };

    ReturnType type;
};

class ASTNODE(Statement) 
{
  CONS_INTER(Statement)
};

typedef std::shared_ptr<Statement> SubStatement;

class STATEMENT(CompoundStatement) {
  public:
    // TODO add inner blocks here
    CompoundStatement(std::vector<BlockItem> subStatements, Pos pos);
    PPRINTABLE
  private:
    std::vector<BlockItem> subStatements;
};

class STATEMENT(ExpressionStatement) {
  public:
    ExpressionStatement(Pos pos);
    ExpressionStatement(SubExpression ex, Pos pos);
    PPRINTABLE

  private:
    SubExpression expression;
};

class ASTNODE(Pointer) {
  public:
    Pointer(int counter, Pos pos) : AstNode(pos), counter(counter) {};
    PPRINTABLE

  private:
    int counter;
};

class STATEMENT(SelectionStatement) {
  public:
    PPRINTABLE
    SelectionStatement(SubExpression ex, SubStatement ifStatement, Pos pos);
    SelectionStatement(
      SubExpression ex, 
      SubStatement ifStatement, 
      SubStatement elseStatement, 
      Pos pos
    );

  // TODO : get rid of hasElseStatement ?
  private:
    SubExpression expression;
    SubStatement ifStatement;
    SubStatement elseStatement;
};

class STATEMENT(JumpStatement) { 
  protected:
    JumpStatement(Pos pos) : Statement(pos) {};
};

class JUMPSTATEMENT(GotoStatement) {
  public:
    PPRINTABLE
    GotoStatement(std::string label, Pos pos);

  private:
    std::string label;
};

class JUMPSTATEMENT(ContinueStatement) {
  public:
    PPRINTABLE
    ContinueStatement(Pos pos);
};

class JUMPSTATEMENT(BreakStatement) {
  public:
    PPRINTABLE
    BreakStatement(Pos pos);
};

class JUMPSTATEMENT(ReturnStatement) {
  public:
    PPRINTABLE
    ReturnStatement(Pos pos);
    ReturnStatement(SubExpression ex, Pos pos);
  
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
    IterationStatement(SubExpression ex,
        SubStatement st,
        IterationEnum k,
        Pos pos);
    PPRINTABLE

  private:
    SubExpression expression;
    SubStatement statement;
    IterationEnum kind;
};

class STATEMENT(LabeledStatement) {
  public:
    LabeledStatement(std::string str, SubStatement st, Pos pos);
    PPRINTABLE

  private:
    std::string name;
    SubStatement statement;
};

typedef std::shared_ptr<LabeledStatement> SubLabeledStatement;
typedef std::shared_ptr<IterationStatement> SubIterationStatement;


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
    DirectDeclaratorHelp(std::shared_ptr<DirectDeclaratorHelp> help, Pos pos);
    DirectDeclaratorHelp(std::vector<ParameterNode> paramList, Pos pos);
    DirectDeclaratorHelp(std::vector<ParameterNode> paramList,
        std::shared_ptr<DirectDeclaratorHelp> help,
        Pos pos);
    DirectDeclaratorHelp(SubIdentifierList idList, Pos pos);
    DirectDeclaratorHelp(SubIdentifierList idList, 
                         std::shared_ptr<DirectDeclaratorHelp> help,
                         Pos pos);
    PPRINTABLE
  private:
    DirectDeclaratorHelpEnum helperType;
    std::shared_ptr<DirectDeclaratorHelp> help;
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
};

typedef std::shared_ptr<DirectDeclarator> SubDirectDeclarator;

class ASTNODE(Declarator) {
  public:
    Declarator(int cnt, SubDirectDeclarator ast, Pos pos);
    PPRINTABLE
  private:
    int pointerCounter;
    SubDirectDeclarator directDeclarator;
};

typedef std::shared_ptr<Declarator> SubDeclarator;

class DIRECTDECLARATOR(IdentifierDirectDeclarator) { 
  public:
   
    IdentifierDirectDeclarator(std::string str,
        SubDirectDeclaratorHelp h,
        Pos pos);

    IdentifierDirectDeclarator(std::string str, Pos pos);

    PPRINTABLE
  // TODO pretty Print
  // handle SubDirectDeclaratorHelp not given
  
  private:
    std::string identifier;
    SubDirectDeclaratorHelp help;
};

class DIRECTDECLARATOR(DeclaratorDirectDeclarator) { 
  public:
    DeclaratorDirectDeclarator(SubDeclarator d,
        SubDirectDeclaratorHelp h,
        Pos pos);
    PPRINTABLE
    // TODO pretty Print
 DeclaratorDirectDeclarator(SubDeclarator d, Pos pos); 
    
  private:
    SubDeclarator declarator;
    SubDirectDeclaratorHelp help;
};

typedef std::shared_ptr<Type> TypeNode;

class ASTNODE(Declaration) {
  public:
    Declaration(TypeNode t, SubDeclarator declarator, Pos pos);
    Declaration(TypeNode t, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
};

typedef std::shared_ptr<Declaration> DeclarationNode;

class ASTNODE(ExternalDeclaration) {
  public:
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        SubCompoundStatement compoundStatement,
                        Pos pos);
    ExternalDeclaration(TypeNode type,
                        SubDeclarator declarator,
                        Pos pos);
    ExternalDeclaration(TypeNode type, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
    SubCompoundStatement compoundStatement;
};

typedef std::shared_ptr<ExternalDeclaration> ExternalDeclarationNode;

class ASTNODE(TranslationUnit) {
  public:
    TranslationUnit(std::vector<ExternalDeclarationNode> externalDeclarations, 
        Pos pos);
    PPRINTABLE
  private:
    std::vector<ExternalDeclarationNode> externalDeclarations;
};

typedef std::shared_ptr<TranslationUnit> TUNode;

class ASTNODE(Parameter) {
  public:
    Parameter(TypeNode type, SubDeclarator declarator, Pos pos);
    Parameter(TypeNode type, Pos pos);
    PPRINTABLE
  private:
    TypeNode type;
    SubDeclarator declarator;
};

typedef std::shared_ptr<Parameter> ParameterNode;

/*

class ASTNODE(Declarator) {
  public:
    Declarator(Pointer ptr, DirectDeclarator) : name(str), statement(st) { };
    PPRINTABLE


}
*/

typedef std::vector<std::pair<TypeNode, std::vector<std::pair<SubDeclarator,SubExpression>>>> StructContent;

class TYPE(StructType) {
  // TODO Add content
  public:
    StructType(Pos pos);
    StructType(std::string name, Pos pos);
    StructType(std::string name, StructContent content, Pos pos);
    bool canBeInFunctionDeclaration() { return !hasDeclaration; };
    PPRINTABLE

  private:
    std::string name;
    StructContent content; 
    bool hasDeclaration;
};

typedef std::shared_ptr<StructType> StructNode;

class EXPRESSION(SizeOfExpression)
{
  public:
    SizeOfExpression(std::pair<TypeNode, SubDeclarator>, Pos pos);
    PPRINTABLE
  private:
    std::pair<TypeNode, SubDeclarator> operand;
};

}

#undef PPRINTABLE
#endif
