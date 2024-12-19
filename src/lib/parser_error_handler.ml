[@@@coverage off]

exception ParserError of string * Lexing.position

let raise_parser_error (msg: string) (startpos: Lexing.position) = 
  let line = startpos.pos_lnum in
  let col = startpos.pos_cnum - startpos.pos_bol + 1 in
  raise (ParserError (Printf.sprintf "Syntax error at line %d, column %d: %s" line col msg, startpos))

[@@@coverage on]
