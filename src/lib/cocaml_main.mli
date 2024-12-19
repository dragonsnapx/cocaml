(* Helper types *)
type parse_result = (Cocaml.Syntax_node.prog, string) result
type compile_result = (string, string) result

(** Compile a file with flags, output a file depending on flags.
    Flags are passed to the LLVM compiler, though there are additional flags available:
    - --to-llvm-ir : Only outputs LLVM IR, must not be combined with other flags.
    - --preprocess-only : Preprocesses the file and outputs the preprocessed result.
    Returns a compile_result indicating the success or failure of the compilation.
*)
val compile : string -> compile_result

(** Parse C code into a C AST tree, returning a result containing the AST or an error message. *)
val parse_c_to_ast : string -> parse_result

(** Generate LLVM IR from an AST, outputting to the specified file. 
    Returns a compile_result indicating success or failure.
*)
val generate_llvm_from_ast : Cocaml.Syntax_node.prog -> string -> compile_result

(** Generate LLVM IR from a C file, returning the result of the generation process. *)
val generate_llvm : string -> compile_result

(** Compile LLVM code using the LLVM backend, outputting an executable.
    Takes the LLVM IR file and the desired output executable name.
    Returns a compile_result indicating success or failure.
*)
val compile_llvm_to_exec : string -> string -> compile_result