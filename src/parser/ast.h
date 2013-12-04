#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <iosfwd>
#include <ostream>
#include <memory>
#include "../lexer/operatortype.h"

#define IS_ASTNODE : public AstNode

        namespace Parser {

        template<typename T> void pprint(T node, std::ostream & out) {
          node.prettyPrint(out);
        }

        template<> void pprint<OperatorType>(OperatorType op, std::ostream & out) {
          switch (op) {
            case OperatorType::PLUS:
              out << " + ";
              break;
            case OperatorType::MINUS:
              out << " - ";
              break;
            case OperatorType::STAR:
              out << " * ";
              break;
            case OperatorType::ASSIGN:
              out << " = ";
              break;
            case OperatorType::EQUAL:
              out << " == ";
              break;
            case OperatorType::QMARK:
              out << " ? ";
              break;
            case OperatorType::COLON:
              out << " : ";
              break;
            case OperatorType::LAND:
              out << " && ";
              break;
            case OperatorType::LOR:
              out << " || ";
              break;
            case OperatorType::LESS:
              out << " < ";
            default:
              out << "ERROR"; //TODO
              break;
          }
        }

template<> void pprint<char>(char c, std::ostream & out) {
  out << c;
}

class AstNode
{
  public:
    virtual ~AstNode() {};
    virtual void prettyPrint(std::ostream &) {};
};

class BinaryExpression IS_ASTNODE
{
  public:
    BinaryExpression(std::shared_ptr<Parser::AstNode> lhs,
                     std::shared_ptr<Parser::AstNode> rhs,
                     OperatorType op);
    void prettyPrint(std::ostream & out) override;
  private:
    std::shared_ptr<Parser::AstNode> lhs;
    std::shared_ptr<Parser::AstNode> rhs;
    OperatorType op;
};

}
#endif
