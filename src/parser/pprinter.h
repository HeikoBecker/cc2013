#ifndef PARSER_PPRINTER_H
#define PARSER_PPRINTER_H
#include <memory>
#include <string>

class AstNode;

namespace Parsing {

void pprint(std::shared_ptr<AstNode> nodeptr, unsigned int indentLevel);

}
#endif
