#ifndef PARSER_AST_H
#define PARSER_AST_H
#pragma once
#include <iosfwd>
#include <ostream>
#include <memory>
#include "../lexer/punctuatortype.h"

#define IS_ASTNODE : public AstNode

        namespace Parser {

        template<typename T> void pprint(T node, std::ostream & out) {
          node.prettyPrint(out);
        }

        template<typename S> void pprint(std::shared_ptr<S> nodeptr,
                                         std::ostream & out) {
          nodeptr->prettyPrint(out);
        }

        template<> void pprint<PunctuatorType>(PunctuatorType op, std::ostream & out) {
          switch (op) {
            case PunctuatorType::PLUS:
              out << " + ";
              break;
            case PunctuatorType::MINUS:
              out << " - ";
              break;
            case PunctuatorType::STAR:
              out << " * ";
              break;
            case PunctuatorType::ASSIGN:
              out << " = ";
              break;
            case PunctuatorType::EQUAL:
              out << " == ";
              break;
            case PunctuatorType::QMARK:
              out << " ? ";
              break;
            case PunctuatorType::COLON:
              out << " : ";
              break;
            case PunctuatorType::LAND:
              out << " && ";
              break;
            case PunctuatorType::LOR:
              out << " || ";
              break;
            case PunctuatorType::LESS:
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
                     PunctuatorType op);
    void prettyPrint(std::ostream & out) override;
  private:
    std::shared_ptr<Parser::AstNode> lhs;
    std::shared_ptr<Parser::AstNode> rhs;
    PunctuatorType op;
};

}
#endif
