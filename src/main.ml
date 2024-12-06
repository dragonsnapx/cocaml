open Core

let parse_c_to_ast filename =
  let lexbuf = Lexing.from_channel (In_channel.create filename) in
  Menhir_parser.program Lexer.token lexbuf