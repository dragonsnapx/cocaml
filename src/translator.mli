module L = Llvm
module S = Syntax_node

module type TranslateFile =
    sig
      val expr_to_vartype : S.expr -> S.vartype

      val extract_expr_value : S.expr -> L.llvalue

      val parse_expr : S.expr -> L.llvalue -> L.llvalue

      val parse_decl : S.decl -> L.llvalue -> unit

      val parse_stmt : S.stmt -> L.llvalue -> L.llvalue

      val generate_llvm_ir : S.prog -> L.llvalue list

      val print_module_to_file : string -> unit
    end