open Core
open Cocaml
open Lexing
open Parser_error_handler 
open Translator.TranslateFile

(* Types for passing along computations *)
type parse_result = (Syntax_node.prog, string) result
type compile_result = (string, string) result

(* Parse a C file into an AST *)
let parse_c_to_ast (filename: string) : parse_result =
  let lexbuf = from_channel (In_channel.create filename) in
  try
    Ok (Menhir_parser.program Lexer.token lexbuf)
  with
    | Menhir_parser.Error ->
        let pos = lexbuf.lex_curr_p in
        let line = pos.pos_lnum in
        let col = pos.pos_cnum - pos.pos_bol + 1 in
        Error (Printf.sprintf "Syntax error at line %d, column %d" line col)
    | ParserError (msg, _) -> 
        Error (Printf.sprintf "Parser error: %s" msg)

(* Generate LLVM IR from an AST *)
let generate_llvm_from_ast (prog: Syntax_node.prog) (output_file: string) : compile_result =
  if Syntax_node.is_prog_empty prog then
    Error "AST is empty! Cannot generate LLVM IR."
  else begin
    let _ = generate_llvm_ir prog in
    print_module_to_file output_file;
    Ok output_file
  end

(* Generate LLVM IR from a file *)
let generate_llvm (filename: string) : compile_result =
  match parse_c_to_ast filename with
  | Ok prog -> generate_llvm_from_ast prog "./result.ll"
  | Error msg -> Error msg

(* Compile LLVM IR file into an executable *)
let compile_llvm_to_exec (llvm_file: string) (output_exec: string) : compile_result =
  let asm_file = "./result.s" in
  match Sys_unix.command ("llc " ^ llvm_file) with
  | 0 -> (
      match Sys_unix.command ("clang " ^ asm_file ^ " -o " ^ output_exec) with
      | 0 -> Ok output_exec
      | _ -> Error (Printf.sprintf "Clang failed to compile %s" asm_file))
  | _ -> Error (Printf.sprintf "LLC failed to compile %s" llvm_file)

(* Conduct full compilation pipeline, from .c to executable *)
let compile (filename: string) : compile_result =
  match generate_llvm filename with
  | Ok llvm_file -> compile_llvm_to_exec llvm_file "result_exec"
  | Error msg -> Error msg

  