open Core
open Cocaml

let unimplemented () = ()

let compile (_filename: string) (_flags: (string * string) list) = 
  unimplemented()

let parse_c_to_ast (filename: string) : Syntax_node.prog =
  let lexbuf = Lexing.from_channel (In_channel.create filename) in
  try
    Menhir_parser.program Lexer.token lexbuf
  with
  | Menhir_parser.Error ->
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "Syntax error at line %d, column %d: unexpected token '%s'\n"
      line col (Lexing.lexeme lexbuf);
    exit 1

let compile_llvm (_filename: string) : bool =
  false;