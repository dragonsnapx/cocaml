open Core

let parse_c_to_ast (filename: string) : Syntax_node.prog =
  let lexbuf = Lexing.from_channel (In_channel.create filename) in
  Menhir_parser.program Lexer.token lexbuf