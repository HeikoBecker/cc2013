#ifndef UTILS_DEBUG_H
#define UTILS_DEBUG_H
#pragma once

#include <iostream>
#include <sstream>

typedef enum debugFilter {
  LEXER   = 0x001,
  PARSER  = 0x002,
  PRINT_AST     = 0x003,
  CODEGEN = 0x004,
  GENERAL = 0x005,
} debugFilter;

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
