#include "punctuatortype.h"
#include <map>

namespace Lexing {
std::string PunctuatorType2String(PunctuatorType pt) {
  static const std::map<PunctuatorType,std::string> t2s {
    {PunctuatorType::PLUS, "+"},
    {PunctuatorType::MINUS, "-"},
    {PunctuatorType::STAR, "*"},
    {PunctuatorType::ASSIGN, "="},
    {PunctuatorType::EQUAL, "=="},
    {PunctuatorType::NEQUAL, "!="},
    {PunctuatorType::QMARK, "?"},
    {PunctuatorType::COLON, ":"},
    {PunctuatorType::SEMICOLON, ";"},
    {PunctuatorType::LAND, "&&"},
    {PunctuatorType::LOR, "||"},
    {PunctuatorType::LESS, "<"},
    {PunctuatorType::GREATER, ">"},
    {PunctuatorType::ARRAY_ACCESS, "[]"},
    {PunctuatorType::MEMBER_ACCESS, "."},
    {PunctuatorType::ARROW, "->"},
    {PunctuatorType::LEFTSQBRACKET, "["},
    {PunctuatorType::RIGHTSQBRACKET, "]"},
    {PunctuatorType::LEFTPARENTHESIS, "("},
    {PunctuatorType::RIGHTPARENTHESIS, ")"},
    {PunctuatorType::LEFTCURLYBRACE, "{"},
    {PunctuatorType::RIGHTCURLYBRACE, "}"},
    {PunctuatorType::COMMA, ","},
    {PunctuatorType::AMPERSAND, "&"},
    {PunctuatorType::NOT, "!"},
  };
  return t2s.at(pt);
}
}
