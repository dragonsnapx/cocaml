open Core
open Cocaml_main
open Cocaml.Syntax_node

let usage_msg = 
  "Usage: cocaml [options] <filename>\n\
   Options:\n\
   --to-llvm-ir           : Only outputs the LLVM IR.\n\
   --preprocess-only      : Outputs the preprocessed result of the input C file.\n\
   --compile              : Compiles the C file into an executable (default behavior).\n\
   --llvm                 : Compiles LLVM IR code to an executable.\n\
   --help                 : Show this help message."

let print_help () =
  print_endline usage_msg

(* These functions just wrap ones in cocaml_main *)
let preprocess_file (filename : string) =
  Printf.printf "Preprocessing file: %s\n" filename;
  let ast = parse_c_to_ast filename in
  Printf.printf "Preprocessed AST:\n%s\n" (Sexp.to_string_hum (sexp_of_prog ast))

let generate_llvm_ir (filename : string) =
  Printf.printf "Generating LLVM IR for file: %s\n" filename;
  compile filename [("--to-llvm-ir", "")];
  Printf.printf "LLVM IR generation complete.\n"

let compile_file (filename : string) =
  Printf.printf "Compiling file: %s\n" filename;
  compile filename [];
  Printf.printf "Compilation successful.\n"

let compile_llvm_file (filename : string) =
  Printf.printf "Compiling LLVM IR file: %s\n" filename;
  let success = compile_llvm filename in
  if success then
    Printf.printf "LLVM compilation successful.\n"
  else
    Printf.printf "LLVM compilation failed.\n"

(* Main Entry Point *)
let () =
  let args = Sys.get_argv () |> Array.to_list in
  match args with
  | _ :: "--to-llvm-ir" :: filename :: _ -> generate_llvm_ir filename
  | _ :: "--preprocess-only" :: filename :: _ -> preprocess_file filename
  | _ :: "--llvm" :: filename :: _ -> compile_llvm_file filename
  | _ :: "--help" :: _ -> print_help ()
  | _ :: filename :: _ -> compile_file filename (* Default behavior is to compile *)
  | _ ->
      Printf.eprintf "Error: No input file specified.\n%s\n" usage_msg
