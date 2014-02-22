#include "debug.h"
#ifdef DEBUG
#include <cstdlib>

debug::debug(debugFilter messageType) : m_messageType(messageType)  {
}

static size_t filter =   (std::getenv("LEXER")     ? LEXER     : SILENT)
                       | (std::getenv("PARSER")    ? PARSER    : SILENT)
                       | (std::getenv("PRINT_AST") ? PRINT_AST : SILENT)
                       | (std::getenv("SEMANTIC")  ? SEMANTIC  : SILENT)
                       | (std::getenv("CODEGEN")   ? CODEGEN   : SILENT)
                       | (std::getenv("GENERAL")   ? GENERAL   : SILENT);

debug::~debug() {
  if (m_messageType & filter) {
    switch (m_messageType) {
      case GENERAL:
        std::cerr << "\033[1;34m";
        std::cerr << "Info: ";
        break;
      case LEXER:
        std::cerr << "\033[1;33m";
        std::cerr << "Lexer: ";
        break;
      case PARSER:
        std::cerr << "\033[1;32m";
        std::cerr << "Parser: ";
        break;
      case SEMANTIC:
        std::cerr << "\033[1;35m";
        std::cerr << "SEMANTIC: ";
        break;
      case CODEGEN:
        std::cerr << "\033[1;31m";
        std::cerr << "Codegen: ";
        break;
      case PRINT_AST:
        std::cerr << "\033[1;30m";
        std::cerr << "AST: ";
        break;
      default:
        break;
    }
    std::cerr << "\033[0m";
    std::cerr << m_SS.str() << std::endl;
  }
}
#else
#endif
