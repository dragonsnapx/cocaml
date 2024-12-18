exception ParserError of string * Lexing.position

val raise_parser_error : string -> Lexing.position -> 'a