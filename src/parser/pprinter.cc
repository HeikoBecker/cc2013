#include <iostream>
#include "pprinter.h"

using namespace Parsing;

PrettyPrinter::PrettyPrinter() : indentLevel(0), out(std::cout) {}

void PrettyPrinter::addIndentLevel() {++indentLevel;}
void PrettyPrinter::removeIndentLevel() {--indentLevel;}
