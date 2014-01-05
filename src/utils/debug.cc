#include "debug.h"

#ifdef DEBUG
debug::debug(debugFilter messageType) : m_messageType(messageType)  {
}

debug::~debug() {
      switch (m_messageType) {
        case LEXER:
          std::cerr << "\033[1;33m";
          std::cerr << "Lexer: ";
          break;
        case PARSER:
          std::cerr << "\033[1;32m";
          std::cerr << "Parser: ";
          break;
        case CODEGEN:
          std::cerr << "\033[1;31m";
          std::cerr << "Codegen: ";
          break;
      }
      std::cerr << "\033[0m";
      std::cerr << m_SS.str() << std::endl;
    }
#else
#endif
