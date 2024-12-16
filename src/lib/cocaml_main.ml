open Core
open Cocaml

let unimplemented () = ()

let compile (_filename: string) (_flags: (string * string) list) = 
  unimplemented()

let parse_c_to_ast (filename: string) : Syntax_node.prog =
  let lexbuf = Lexing.from_channel (In_channel.create filename) in
  Menhir_parser.program Lexer.token lexbuf

let compile_llvm (_filename: string) : bool =
  false;