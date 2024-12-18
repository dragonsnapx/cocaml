module L = Llvm
module S = Syntax_node

module TranslateFile :
    sig
        val ident_to_string : S.Ident.t -> string

        val expr_to_vartype : S.Expr.t -> S.VarType.t

        val extract_expr_value : S.Expr.t -> L.llvalue

        val parse_expr : S.Expr.t -> L.llvalue -> L.llvalue

        val parse_decl : S.Decl.t -> L.llvalue option -> L.llvalue

        val parse_stmt : S.Stmt.t -> L.llvalue -> L.llvalue

        val generate_llvm_ir : S.prog -> L.llvalue list

        val print_module_to_file : string -> unit

        val string_of_file : string
    end