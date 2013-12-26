#ifndef PARSER_EXCEPTION_H
#define PARSER_EXCEPTION_H


namespace Parsing {

  class ParsingException: public std::runtime_error 
  {
    public:
      ParsingException(std::string message, Pos pos) 
        : std::runtime_error(message), pos(pos) {};
      Pos pos;
  };

}


#endif