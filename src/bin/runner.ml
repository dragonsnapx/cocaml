open Core
open Cocaml_main
open Cocaml.Syntax_node

let usage_msg = 
  "Usage: ./runner.exe [options] <filename>\n\
   Options:\n\
   --compile              : Compiles the C file into an executable (default behavior).\n\
   --preprocess-only      : Outputs the preprocessed result of the input C file.\n\
   --to-llvm-ir           : Only outputs the LLVM IR.\n\
   --llvm                 : Compiles LLVM IR code to an executable.\n\
   --help                 : Show this help message."

let print_help () =
  print_endline usage_msg

(* These functions just wrap the ones in cocaml_main *)
let preprocess_file (filename : string) =
  Printf.printf "Preprocessing file: %s\n" filename;
  match parse_c_to_ast filename with
  | Ok prog ->
      Printf.printf "Preprocessed AST:\n%s\n" (Sexp.to_string_hum (sexp_of_prog prog))
  | Error prog ->
      Printf.eprintf "Error during preprocessing: %s\n" prog

let generate_llvm_ir (filename : string) =
  Printf.printf "Generating LLVM IR for file: %s\n" filename;
  match generate_llvm filename with
  | Ok llvm_file ->
      Printf.printf "LLVM IR generated successfully!\nOutput:  %s\n" llvm_file
  | Error msg ->
      Printf.eprintf "Error generating LLVM IR: %s\n" msg

let compile_llvm_file (filename : string) =
  Printf.printf "Compiling LLVM IR file: %s\n" filename;
  match compile_llvm_to_exec filename "result_exec" with
  | Ok exec_file ->
      Printf.printf "LLVM IR compiled successfully!\nExecutable: %s\n" exec_file
  | Error msg ->
      Printf.eprintf "Error compiling LLVM IR: %s\n" msg

let compile_file (filename : string) =
  Printf.printf "Compiling file: %s\n" filename;
  match compile filename with
  | Ok exec_file ->
      Printf.printf "Compilation successful! Executable: %s\n" exec_file
  | Error msg ->
      Printf.eprintf "Error during compilation: %s\n" msg

(* Main Entry Point *)
let () =
  let args = Sys.get_argv () |> Array.to_list in
  match args with
  | _ :: "--compile" :: filename :: _ -> compile_file filename
  | _ :: "--preprocess-only" :: filename :: _ -> preprocess_file filename
  | _ :: "--to-llvm-ir" :: filename :: _ -> generate_llvm_ir filename
  | _ :: "--llvm" :: filename :: _ -> compile_llvm_file filename
  | _ :: "--help" :: _ -> print_help ()
  | _ :: filename :: _ -> compile_file filename (* Default behavior is to compile *)
  | _ ->
      Printf.eprintf "Error: No input file specified.\n%s\n" usage_msg
