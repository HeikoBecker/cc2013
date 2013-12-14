#include <iostream>
#include "pprinter.h"

using namespace Parsing;

PrettyPrinter::PrettyPrinter() : indentLevel(0) {}

void PrettyPrinter::addIndentLevel() {++indentLevel;}
void PrettyPrinter::removeIndentLevel() {--indentLevel;}
