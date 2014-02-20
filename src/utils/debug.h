#ifndef UTILS_DEBUG_H
#define UTILS_DEBUG_H
#pragma once

#include <iostream>
#include <sstream>


typedef enum debugFilter {
  SILENT = 0x000,
  LEXER   = 1 << 0,
  PARSER  = 1 << 1,
  PRINT_AST     = 1 << 2,
  CODEGEN = 1 << 3,
  GENERAL = 1 << 4,
} debugFilter;

constexpr debugFilter filter = SILENT;

#ifdef DEBUG
struct debug {
    debug(debugFilter messageType);
    ~debug();

    template<class T>
    debug &operator<<(const T &x) {
        m_SS << x;
        return *this;
    }
private:
    std::ostringstream m_SS;
    debugFilter m_messageType;
};
#else
struct debug {
   debug(debugFilter messageType) {(void) messageType;}
    template<class T>
    debug &operator<<(const T &) {
        return *this;
    }
};
#endif
#endif
