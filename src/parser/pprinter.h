#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <string>
#include "astNode.h"
#include "../lexer/token.h"
#include "../utils/debug.h"
#include "../lexer/punctuatortype.h"

//enum class PunctuatorType;

namespace Parsing {

void pprint(AstNode node, unsigned int indentLevel);

void pprint(std::shared_ptr<AstNode> nodeptr, unsigned int indentLevel);

void pprint(PunctuatorType op, unsigned int indentLevel);

void pprint(char c, unsigned int indentLevel);

void pprint(std::string s, unsigned int indentLevel);

}
#endif
