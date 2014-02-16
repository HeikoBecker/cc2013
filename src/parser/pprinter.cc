#include <iostream>
#include "ast.h"
#include "pprinter.h"
#include "../lexer/punctuatortype.h"

namespace Parsing {

void pprint(AstNode node, unsigned int indentLevel) {
  node.prettyPrint(indentLevel);
}

void pprint(std::shared_ptr<AstNode> nodeptr, unsigned int indentLevel)
{
  if(nodeptr) {
    nodeptr->prettyPrint(indentLevel);
  } else {
    pprint(std::string("\n"), indentLevel);
    pprint(std::string("******************************************\n"),
        indentLevel);
    pprint(std::string("*WARNING: SHARED_PTR WAS NOT INITIALIZED!*\n"),
        indentLevel);
    pprint(std::string("*Should never happen at this stage!"),
        indentLevel);
    pprint(std::string("******************************************\n"),
        indentLevel);
    pprint(std::string("\n"),
        indentLevel);
  }
}

void pprint(PunctuatorType op, unsigned int indentLevel)
{
  (void) indentLevel;
  std::cout << Lexing::PunctuatorType2String(op);
}

void pprint(char c, unsigned int indentLevel)
{
  switch (c) {
    case '\n':
      std::cout << '\n';
      for (int i = indentLevel; i > 0; i--) {
        std::cout << '\t';
      }
      break;
    default:
      std::cout << c;
      break;
  }
}

void pprint(std::string s, unsigned int indentLevel)
{
  (void) indentLevel;
  // TODO: replace newline in s with newline + indentLevel times tab char
  std::cout << s;
}

}

using namespace Parsing;

bool g_skipNewLineBeforeBlockStatement = false; // TODO: FIXME: global variables are BAD!
bool g_skipNewLineBeforeSelectionStatement = false; // TODO: FIXME: global variables are BAD!
bool g_adjustParenthesesInDeclarator = false; // TODO: FIXME: global variables are BAD!

#define PRETTY_PRINT(X) void X::prettyPrint(unsigned int indentLevel)
/* Beware of macro magic
 * This prints the name of function calling PPRINT, it's argument and finally
 * any output produced by the function
 */
#define PPRINT(X)  do { \
  debug(PRINT_AST) << __PRETTY_FUNCTION__ << "\t: "\
        << std::string(#X) << ":";\
  pprint((X), indentLevel);\
} while(0);
#define ADDINDENT()  do {indentLevel++;} while(0)
#define REMOVEINDENT()  do {indentLevel--;} while(0)
#define RESETINDENT() \
  auto reset = indentLevel;\
  indentLevel = 0;
#define RESTOREINDENT() indentLevel = reset;


PRETTY_PRINT(BinaryExpression)
{
  PPRINT('(');
  PPRINT(this->lhs);
  switch (this->op) {
    case PunctuatorType::ARRAY_ACCESS:
      PPRINT('[');
      PPRINT(this->rhs);
      PPRINT(']');
      PPRINT(')');
      return;
      //break;
    case PunctuatorType::ARROW:
    case PunctuatorType::MEMBER_ACCESS:
      PPRINT(op);
      break;
    default:
      PPRINT(' ');
      PPRINT(op);
      PPRINT(' ');
      break;
  }
  PPRINT(this->rhs);
  PPRINT(')');
}

PRETTY_PRINT(UnaryExpression)
{
  PPRINT('(');
  PPRINT(this->op);
  if (this->op == PunctuatorType::SIZEOF && !(std::dynamic_pointer_cast<SizeOfExpression>(operand))) {
    PPRINT(' ');
  }
  PPRINT(this->operand);
  PPRINT(')');
}

PRETTY_PRINT(VariableUsage)
{
  PPRINT(this->name);
}

PRETTY_PRINT(Literal)
{
  PPRINT(this->name);
}

PRETTY_PRINT(Constant)
{
  PPRINT(this->name);
}

PRETTY_PRINT(FunctionCall)
{
  PPRINT('(');
  PPRINT(funcName);
  PPRINT('(');
  if (!arguments.empty()) {
    auto size = arguments.size();
      for (auto argument : arguments) {
        PPRINT(argument);
        if (--size != 0) {
          PPRINT(',');
          PPRINT(' ');
        }
      }
  }
  PPRINT(')');
  PPRINT(')');
}

PRETTY_PRINT(TernaryExpression)
{
  PPRINT('(');
  PPRINT(this->condition);
  PPRINT(std::string(" ? "));
  PPRINT(this->lhs);
  PPRINT(std::string(" : "));
  PPRINT(this->rhs);
  PPRINT(')');
}

PRETTY_PRINT(BasicType)
{
  switch(type) {
    case INT:
      PPRINT(std::string("int"));
      break;
    case CHAR:
      PPRINT(std::string("char"));
      break;
    default :
      /*case VOID: */
      PPRINT(std::string("void"));
      break;
  }
}

PRETTY_PRINT(StructType) {
  // TODO: this looks way too complicated
  PPRINT(std::string("struct "));

  if (name.length() > 0) {
    PPRINT(name);
  }

  if (!content.empty()) {
    PPRINT('\n');
    PPRINT('{');
    ADDINDENT();

    for (auto typeSubDeclarationPair : content) {
      PPRINT('\n');
      PPRINT(typeSubDeclarationPair.first);
      if (!typeSubDeclarationPair.second.empty()) {
        PPRINT(' '); // print space between type and following stuff
      } else {
        PPRINT(';');
      }
      for (auto subDeclarationPair : typeSubDeclarationPair.second) {
        PPRINT(subDeclarationPair.first);
        if (subDeclarationPair.second) {
          //PPRINT(' '); //TODO: this seems wrong
          PPRINT(subDeclarationPair.second);
        }
        PPRINT(';'); // <- FIXME: no idea if this belongs here, but probably correct
      }
    }
    REMOVEINDENT();
    PPRINT('\n');
    PPRINT('}');
  }
}


PRETTY_PRINT(CompoundStatement) {
  // TODO: unfinished, add special case for last statement regarding newline
  if (!g_skipNewLineBeforeBlockStatement) {
    PPRINT('\n');
  } else {
    PPRINT(' ');
    g_skipNewLineBeforeBlockStatement = false; //FIXME: global variable!
  }
  PPRINT('{');
  ADDINDENT();
  for (auto statement : subStatements) {
    PPRINT(statement);
  }
  REMOVEINDENT();
  PPRINT('\n');
  PPRINT('}');
}

PRETTY_PRINT(Pointer) {
  for(int n=0; n<counter; n++) {
    PPRINT('*');
  }
}

PRETTY_PRINT(ExpressionStatement) {
  // TODO : check whether this is working
  PPRINT('\n');
  if (expression != NULL) {
    PPRINT(expression);
  }
  PPRINT(';');
}

PRETTY_PRINT(SelectionStatement) {
  auto suppressedIndent = false;
  if (g_skipNewLineBeforeSelectionStatement) {
    g_skipNewLineBeforeSelectionStatement = false;
    PPRINT(' ');
  } else {
    PPRINT('\n');
  }
  PPRINT(std::string("if ("));
  PPRINT(expression);
  PPRINT(')');
  if (std::dynamic_pointer_cast<CompoundStatement>(ifStatement)) {
    suppressedIndent = true;
    g_skipNewLineBeforeBlockStatement = true;
  }
  if (!suppressedIndent) {
    ADDINDENT();
  }
  PPRINT(ifStatement);
  if (!suppressedIndent) {
    REMOVEINDENT();
  }

  if (elseStatement) {
    if (suppressedIndent) {
      PPRINT(' ');
      suppressedIndent = false;
    } else {
      PPRINT('\n');
    }
    PPRINT(std::string("else"));
    // don't set a new indent level if we have a selection statement
    if (std::dynamic_pointer_cast<SelectionStatement>(elseStatement)) {
      g_skipNewLineBeforeSelectionStatement = true;
      suppressedIndent = true;
    } else if (std::dynamic_pointer_cast<CompoundStatement>(elseStatement)) {
      suppressedIndent = true;
      g_skipNewLineBeforeBlockStatement = true;
    } else {
      ADDINDENT();
    }
    PPRINT(elseStatement);
    if (!suppressedIndent) {
      REMOVEINDENT(); // only remove indentLevel if we have added one
    }
  }
}

PRETTY_PRINT(GotoStatement) {
  PPRINT('\n');
  PPRINT(std::string("goto "));
  PPRINT(label);
  PPRINT(';');
}

PRETTY_PRINT(ContinueStatement) {
  PPRINT('\n');
  PPRINT(std::string("continue;"));
}

PRETTY_PRINT(BreakStatement) {
  PPRINT('\n');
  PPRINT(std::string("break;"));
}

PRETTY_PRINT(ReturnStatement) {
  PPRINT('\n');
  PPRINT(std::string("return"));
  if (expression != NULL) {
    PPRINT(' ');
    PPRINT(expression);
  }
  PPRINT(';');
}

PRETTY_PRINT(IterationStatement) {
  PPRINT('\n');
  if (kind == WHILE) {
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string(")"));
    if (std::dynamic_pointer_cast<CompoundStatement>(statement)) {
      g_skipNewLineBeforeBlockStatement = true;
      PPRINT(statement);
    } else {
      ADDINDENT();
      PPRINT(statement);
      REMOVEINDENT();
    }
  } else { // kind == DO
    PPRINT(std::string("do "));
    PPRINT(statement);
    PPRINT(std::string("while ("));
    PPRINT(expression);
    PPRINT(std::string(");"));
  }
}

PRETTY_PRINT(LabeledStatement) {
  RESETINDENT();
  PPRINT('\n');
  PPRINT(name);
  PPRINT(std::string(":"));
  RESTOREINDENT();
  if (std::dynamic_pointer_cast<LabeledStatement>(statement)) {
    PPRINT(statement);
  } else {
    PPRINT(statement);
  }
}

PRETTY_PRINT(IdentifierList) {
  PPRINT(nameList[0]);
  for(int n = 1; n < (int) nameList.size(); n++) {
    PPRINT(',');
    PPRINT(nameList[n]);
  }
}

PRETTY_PRINT(Declarator)
{
  // TODO : unfinished, probably also broken
  auto adjust = g_adjustParenthesesInDeclarator;
  for (auto i = this->pointerCounter; i>0; --i) {
    if (adjust) {
      adjust = false;
    } else {
      PPRINT('(');
    }
    PPRINT('*');
  }
  adjust = g_adjustParenthesesInDeclarator;
  g_adjustParenthesesInDeclarator = false;
  if (directDeclarator) {
    PPRINT(this->directDeclarator);
  }
  for (auto i = this->pointerCounter; i>0; --i) {
    if (adjust) {
      adjust = false;
    } else {
      PPRINT(')');
    }
  }
}

PRETTY_PRINT(Declaration)
{
  PPRINT('\n');
  PPRINT(type);
  PPRINT(' ');
  if (declarator) {
    PPRINT(declarator);
  }
  PPRINT(';');
}

PRETTY_PRINT(TranslationUnit) {
  auto size = externalDeclarations.size();
  for(auto externalDeclaration : externalDeclarations) {
    PPRINT(externalDeclaration);
    if (size > 1) {
      PPRINT('\n');
      size--;
    }
  }
}

PRETTY_PRINT(ExternalDeclaration) {
  /*TODO: unfinished */
  PPRINT(this->type);
  if (this->declarator) {
    PPRINT(' ');
    PPRINT(this->declarator);
  }
  // TODO: move semicolon printing to declarations
  PPRINT(';'); // declarations end with an ;
  debug(PARSER) << "ExternalDeclaration END";
  PPRINT('\n');
}

PRETTY_PRINT(FunctionDefinition) {
  /*TODO: unfinished */
  PPRINT(this->type);
  PPRINT(' ');
  PPRINT('('); // <- WTF, that's a really coding style
  g_adjustParenthesesInDeclarator = true;
  PPRINT(this->declarator);
  PPRINT(')');
  PPRINT(this->compoundStatement);
  PPRINT('\n');
  debug(PARSER) << "ExternalDeclaration END";
}



PRETTY_PRINT(IdentifierDirectDeclarator) {
  /*TODO: unfinished*/
  PPRINT(this->identifier);
  auto numDiDeHelp = help.size();
  // TODO: we might need to print helpers which are not the last one in
  // parentheses
  while (numDiDeHelp) {
    PPRINT(help.at(--numDiDeHelp));
  }
}

PRETTY_PRINT(Parameter) {
  /*TODO: unfinished*/
  PPRINT(type);
  if (declarator) {
    PPRINT(' ');
    PPRINT(declarator);
  }
}


PRETTY_PRINT(DeclaratorDirectDeclarator)
{
  // TODO: broken
  size_t numDiDeHelp = help.size();
  if (numDiDeHelp > 0) {
    PPRINT('(');
  }
  PPRINT(declarator);
  if (numDiDeHelp > 0) {
    PPRINT(help.at(--numDiDeHelp));
    while (numDiDeHelp) {
      PPRINT('(');
      PPRINT(help.at(--numDiDeHelp));
      PPRINT(')');
    }
    PPRINT(')');
  }
}

PRETTY_PRINT(DirectDeclaratorHelp)
{
  switch (helperType) {
    case EPSILON:
      // TODO: the EPSILON case should not be necessary anymore
      debug(PARSER) << "should never happen!\n";
      PPRINT('(');
      PPRINT(')');
      return;
    case IDENTIFIERLIST:
      PPRINT(idList);
    case EMPTYLIST:
      PPRINT('(');
      PPRINT(')');
      break;
    case PARAMETERLIST:
      PPRINT('(');
      auto size = paramList.size();
      for (auto parameter: paramList) {
        PPRINT(parameter);
        size--;
        if (size > 0) {
          PPRINT(std::string(", "));
        }
      }
      PPRINT(')');
  }
}  

PRETTY_PRINT(SizeOfExpression)
{
  PPRINT('(');
  PPRINT(operand.first);
  if (operand.second) {
    PPRINT(operand.second);
  }
  PPRINT(')');
}

PRETTY_PRINT(DirectDeclarator)
{
  PPRINT(std::string("Called prettyPrint of DirectDeclarator directly. Why?\n"));
};

#undef PPRINT
#undef ADDINDENT
#undef REMOVEINDENT
#undef PRETTY_PRINT
