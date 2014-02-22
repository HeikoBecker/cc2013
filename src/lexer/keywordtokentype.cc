#include "keywordtokentype.h"
#include <string>
#include <unordered_map>
#include <functional>


namespace std
{
  // C++ doesn't use underlying_type of enum class for the hasher. Why???
    template<>
    struct hash<KeywordType>
    {
        typedef ::KeywordType argument_type;
        typedef std::underlying_type< argument_type >::type underlying_type;
        typedef std::hash< underlying_type >::result_type result_type;
        result_type operator()( const argument_type& arg ) const
        {
            std::hash< underlying_type > hasher;
            return hasher( static_cast< underlying_type >( arg ) );
        }
    };
}


namespace Lexing {
std::string KeywordType2String(const KeywordType kt) {
  static const std::unordered_map<KeywordType, std::string> lookup
  {
    {KeywordType::AUTO, "auto"},
    {KeywordType::BREAK,"break"},
    {KeywordType::CASE, "case"},
    {KeywordType::CHAR,"char"},
    {KeywordType::CONST, "const"},
    {KeywordType::CONTINUE, "continue"},
    {KeywordType::DEFAULT, "default"},
    {KeywordType::DO, "do"},
    {KeywordType::DOUBLE,"double"},
    {KeywordType::ELSE, "else"},
    {KeywordType::ENUM,"enum"},
    {KeywordType::FLOAT,"float"},
    {KeywordType::FOR, "for"},
    {KeywordType::GOTO,"goto"},
    {KeywordType::IF,"if"},
    {KeywordType::INT, "int"},
    {KeywordType::RETURN, "return"},
    {KeywordType::SIZEOF, "sizeof"},
    {KeywordType::STRUCT, "struct"},
    {KeywordType::SWITCH, "switch"},
    {KeywordType::VOID, "void"},
    {KeywordType::WHILE, "while"},
  };
  return lookup.at(kt);
}

}
