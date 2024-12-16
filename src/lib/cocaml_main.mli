
(** Compile a file with flags, output a file depending on flags
    Flags are passed to LLVM compiler, though there are additional flags available:
    --to-llvm-ir : Only outputs LLVM IR, must not be combined with other flags
    --preprocess-only : Preprocesses the file and outputs the preprocessed result
*)
val compile : string -> (string * string) list -> unit

(** Convert C code into C AST Tree *)
val parse_c_to_ast : string -> Cocaml.Syntax_node.prog

(** Compile LLVM code using the LLVM backend, return based on success or failure *)
val compile_llvm : string -> bool