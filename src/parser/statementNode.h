#ifndef PARSER_STATEMENT_H
#define PARSER_STATEMENT_H
#pragma once

#include <memory>
#include "astNode.h"

namespace Parsing {

class Expression; // forward declare
typedef std::shared_ptr<Expression> SubExpression;

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
    IR_EMITTING
  private:
    std::vector<BlockItem> subStatements;
};

class STATEMENT(ExpressionStatement) {
  public:
    ExpressionStatement(Pos pos);
    ExpressionStatement(SubExpression ex, Pos pos);
    PPRINTABLE
    IR_EMITTING

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
    IR_EMITTING
    ReturnStatement(Pos pos);
    ReturnStatement(SubExpression ex, Pos pos);
  
  private:
    SubExpression expression;
    void verifyReturnType(SemanticDeclarationNode actual_type);
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

}

#endif
