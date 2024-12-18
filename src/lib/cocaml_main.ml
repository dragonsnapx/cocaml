open Core
open Cocaml
open Lexing
open Parser_error_handler 
open Translator.TranslateFile

let ignore _ = ()

let parse_c_to_ast (filename: string) : Syntax_node.prog =
  let lexbuf = from_channel (In_channel.create filename) in
  try
    Menhir_parser.program Lexer.token lexbuf
  with
    | Menhir_parser.Error ->
        (* Handle Menhir built-in errors *)
        let pos = lexbuf.lex_curr_p in
        let line = pos.pos_lnum in
        let col = pos.pos_cnum - pos.pos_bol + 1 in
        Printf.eprintf "Syntax error at line %d, column %d\n" line col;
        Syntax_node.Prog []
    | ParserError (msg, _) ->
        Printf.eprintf "ParserError: %s\n" msg;
        Syntax_node.Prog []
        
let compile_llvm (filename: string) : bool =
  parse_c_to_ast filename
  |> generate_llvm_ir
  |> ignore;
  print_module_to_file "./result.ll";
  Sys_unix.command "llc ./result.ll" |> ignore;
  Sys_unix.command "clang ./result.s -o result_exec" |> ignore;
  true

let compile (_filename: string) (_flags: (string * string) list) = 
  compile_llvm _filename |> ignore
  