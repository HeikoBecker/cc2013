#ifndef UTILS_DEBUG_H
#define UTILS_DEBUG_H
#pragma once

#include <iostream>
#include <sstream>


typedef enum debugFilter : size_t {
  SILENT    = 0x000,
  LEXER     = 1 << 0,
  PARSER    = 1 << 1,
  PRINT_AST = 1 << 2,
  SEMANTIC  = 1 << 3,
  CODEGEN   = 1 << 4,
  GENERAL   = 1 << 5,
} debugFilter;

#ifdef DEBUG
/*
 * \brief: A simple class for printing debug messages
 * Note that this class does nothing DEBUG is not defined
 * When running the executable, you have to enable options in the followin way:
 * LEXER=1 SEMANTIC=1 ./build/default/c4 --parse example.c
 * This will enable debug messages tagged with LEXER and SEMANTIC, but won't
 * print anything for the other tags (PARSER, GENERAL, CODEGEN, ...)
 * Usage: Just create an instance of the class
 * and use it like a normal outstream
 * Example: debug(LEXER) << "Lexing is fun!"
 */
struct debug {
    debug(debugFilter messageType = GENERAL);
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
