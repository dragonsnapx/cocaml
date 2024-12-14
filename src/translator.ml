[@@@ocaml.warning "-39-32-27-69"]
open Core
module L = Llvm
module S = Syntax_node

module DefinedFunc = struct
  type t = {
    fn: L.llvalue;
    fntp: L.lltype;
    returns: L.llvalue option;
    returns_to: L.llbasicblock;
  }
end

module DefinedVar = struct
  type t = {
    tp: S.vartype;
    ltp: L.lltype;
    value: L.llvalue;
  }
end

module TranslateFile = 
struct
  let context = L.global_context()
  let builder = L.builder context
  let this_module = L.create_module context "module_name_here"

  let functions: (S.ident, DefinedFunc.t) Hashtbl.t = Hashtbl.Poly.create ()
  let mixins = Hashtbl.create
  let types: (S.ident, L.lltype) Hashtbl.t = Hashtbl.Poly.create ()
  let variables: (S.ident, DefinedVar.t) Hashtbl.t = Hashtbl.Poly.create ()

  let ll_char_t = L.i8_type context
  let ll_int_t = L.i32_type context
  let ll_long_t = L.i64_type context
  let ll_double_t = L.double_type context
  let ll_float_t = L.float_type context

  let nil_return_type = L.const_null (L.i32_type context)

  let ignore_llvalue (v : L.llvalue) : unit =
    ()

  let ident_to_string (ident: S.ident): string =
    match ident with
    | Ident s -> s

  let rec vartype_to_llvartype (tp: S.vartype): L.lltype =
    match tp with
    | Int -> ll_int_t
    | Float -> ll_float_t
    | Double -> ll_double_t
    | Void -> L.void_type context
    | Pointer subtp -> L.pointer_type context
    | Char -> ll_char_t
    | Long -> ll_long_t
    | Typedef custom -> 
      try
        (Hashtbl.find_exn types custom)
      with Not_found_s _ ->
        failwith @@ "Translation Error: Undefined type: " ^ (ident_to_string custom) 

  let expr_to_vartype (expr: S.expr): S.vartype =
    match expr with
    | IntLiteral _ -> S.Int
    | FloatLiteral _ -> S.Float
    | CharLiteral _ -> S.Char
    | Var (ident, _) -> begin
      try (Hashtbl.find_exn variables ident).tp
      with Not_found_s _ -> failwith @@ "Translation Error: Cannot infer type of " ^ (ident_to_string ident)
      end
    | BinOp (bin_op, expr1, expr2, _) -> S.Int
    | Assign (ident, expr, _) -> failwith "TODO"
    | Call (expr, param, _) -> failwith "TODO"
    | PrefixUnOp (prefix_un_op, expr, _) -> failwith "TODO"
    | PostfixUnOp (expr, postfix_un_op, _) -> failwith "TODO"

  let extract_expr_value (expr: S.expr): L.llvalue =
    match expr with
    | IntLiteral (i, _) -> L.const_int (L.i32_type context) i
    | FloatLiteral (f, _) -> L.const_float (L.float_type context) f
    | CharLiteral (c, _) -> L.const_int (L.i8_type context) (int_of_char c)
    | Var (id, _) -> 
      begin
        try (Hashtbl.find_exn variables id).value
        with Not_found_s _  -> failwith @@ "Translation Error: Cannot extract value from " ^ (ident_to_string id)
      end
    | _ -> failwith "Translation Error: extract_expr_value must be called on literal"

  let func_entry_builder (): L.llbuilder =
    L.insertion_block builder
    |> L.block_parent
    |> L.entry_block
    |> L.instr_begin
    |> L.builder_at context


  let rec declare_function (label: S.ident) (returns: L.lltype) (params: (S.vartype * S.ident) list) (body: S.stmt) =
    let params_ll_value = 
      List.map params ~f:(fun elt -> elt |> fst |> vartype_to_llvartype) 
      |> Array.of_list 
    in
    let func_type = L.function_type returns params_ll_value in
    let fn_name = ident_to_string label in
    let fn = match L.lookup_function fn_name this_module with
    | None -> L.declare_function fn_name func_type this_module
    | Some _ -> failwith "Translation Error: Function already exists" in
    let entry_block = L.append_block context "entry" fn in
    L.position_at_end entry_block builder;
    List.iteri params ~f:(fun i (tp, id) -> 
      let ll_param = L.param fn i in
      L.set_value_name (ident_to_string id) ll_param;
      let ll_param_alloc = L.build_alloca (vartype_to_llvartype tp) (ident_to_string id) builder in
      L.build_store ll_param ll_param_alloc builder |> ignore_llvalue;
    );
    parse_stmt body fn |> ignore;
    fn

  and parse_expr (expr: S.expr) (scoped_fn: L.llvalue): L.llvalue =
    match expr with
    | IntLiteral (i, _) -> L.const_int ll_int_t i
    | FloatLiteral (f, _) -> L.const_float ll_float_t f
    | CharLiteral (c, _) -> L.const_int ll_char_t (int_of_char c)
    | Var (v, _) ->
      let ll_v = (Hashtbl.find_exn variables v) in
      L.build_load (ll_v.ltp) (ll_v.value) (ident_to_string v) builder
    | BinOp (bin_op, expr1, expr2, _) -> begin
        let lhs = parse_expr expr1 scoped_fn in
        let rhs = parse_expr expr2 scoped_fn in
        match bin_op with
        | Plus -> L.build_add lhs rhs "add_instr" builder
        | Minus -> L.build_sub lhs rhs "sub_instr" builder
        | Times -> L.build_mul lhs rhs "mul_instr" builder
        | Divide -> L.build_sdiv lhs rhs "div_instr" builder
        | Modulo -> failwith "TODO"
        | Equal -> L.build_icmp L.Icmp.Eq lhs rhs "eq_icmp" builder
        | NotEqual -> L.build_icmp L.Icmp.Ne lhs rhs "neq_icmp" builder
        | Less -> L.build_icmp L.Icmp.Slt lhs rhs "lt_icmp" builder
        | LessEqual -> L.build_icmp L.Icmp.Sle lhs rhs "leq_icmp" builder
        | Greater -> L.build_icmp L.Icmp.Sgt lhs rhs "gt_icmp" builder
        | GreaterEqual -> L.build_icmp L.Icmp.Sge lhs rhs "gte_icmp" builder
        | BitwiseAnd -> L.build_and lhs rhs "and_bit" builder
        | BitwiseOr -> L.build_or lhs rhs "or_bit" builder
        | BitwiseXor -> L.build_xor lhs rhs "xor_bit" builder
        | LogicalAnd -> failwith "TODO"
        | LogicalOr -> failwith "TODO"
      end
    | Assign (id, e, _) -> 
        let ll_v = (Hashtbl.find_exn variables id) in
        L.build_store ll_v.value (parse_expr e scoped_fn) builder
    | Call (id, exprs, _) -> begin
        let args = exprs
        |> List.map ~f:(fun el -> parse_expr el scoped_fn)
        |> Array.of_list in
        match Hashtbl.find functions id with
        | Some fn -> L.build_call fn.fntp fn.fn args "fun_call" builder
        | None -> failwith @@ "Cannot find function call to function: " ^ (ident_to_string id)
      end
    | PrefixUnOp (prefix_un_op, e, _) -> failwith "TODO"
    | PostfixUnOp (e, postfix_un_op, _) -> failwith "TODO"

  and parse_stmt (stmt: S.stmt) (scoped_fn: L.llvalue): L.llvalue =
    match stmt with
    | Return (expr, _) -> L.build_ret (parse_expr expr scoped_fn) builder
    | If (expr, stmt, stmt_body, _) -> failwith "TODO"
    | While (expr, stmt, _) -> begin
      let condition_block = L.append_block context "while.cond" scoped_fn in
      let condition = parse_expr expr scoped_fn in
      let while_block = L.append_block context "while.true" scoped_fn in
      let end_block = L.append_block context "while.end" scoped_fn in
      L.position_at_end condition_block builder;
      let break_end = L.build_cond_br condition while_block end_block builder in
      L.position_at_end while_block builder;
      parse_stmt stmt scoped_fn |> ignore_llvalue;
      L.position_at_end end_block builder;
      break_end
    end
    | For (expr1, expr2, expr3, stmt_body, _) -> failwith "TODO"
    | ExprStmt (expr, _) -> parse_expr expr scoped_fn
    | Block (stmt_body_ls, _) -> 
      let current_block = L.insertion_block builder in
      let scoped_block =
        List.fold_left stmt_body_ls ~init:current_block ~f:(fun _ stmt ->
          parse_stmt stmt scoped_fn |> ignore_llvalue;
          L.insertion_block builder
        )
      in
      L.position_at_end scoped_block builder;
      (* I wanted to return a null type, but that causes an error *)
      nil_return_type
    | Switch (expr, cases, _) -> failwith "TODO"
    | Break _ -> failwith "TODO"
    | Continue _ -> failwith "TODO"
    | DoWhile (stmt, expr, _) -> failwith "TODO"
    | LocalVarDecl (is_static, vartype, ident, expr, position) ->
      let lltype = vartype_to_llvartype vartype in
      let llname = ident_to_string ident in
      let alloca = 
        L.insertion_block builder 
        |> L.block_parent 
        |> L.entry_block
        |> L.instr_begin
        |> L.builder_at context
        |> L.build_alloca lltype llname
      in
      (*Hashtbl maybe?*)
      begin
        match expr with
        | Some expr_ ->
          let value = parse_expr expr_ scoped_fn in
          L.build_store value alloca builder |> ignore_llvalue
        | None -> ()
      end;
      nil_return_type

  let parse_decl (decl: S.decl) (scoped_fn: L.llvalue option): L.llvalue =
    (* Local Variable *)
    match decl with
    (* Global variable *)
    | GlobalVarDecl (is_static, vartype, ident, expr, position) -> begin
        match expr with
        | Some v -> L.declare_global ll_int_t (ident_to_string ident) this_module
        | None -> failwith "Translation Error: Global variable must be initialized"
      end
    | FuncDecl (vartype, ident, params, stmt, _) ->
      declare_function ident (vartype_to_llvartype vartype) params stmt
    | Typedef (from_vartype, to_vartype, position) ->
      begin
        match from_vartype with
        | Pointer _ | Void -> failwith "Translation Error: Cannot typecast pointer or void." 
        | Typedef custom  -> failwith "TODO"
        | v -> failwith "TODO"
      end
    | StructDecl (_, _, _) -> failwith "TODO"

  (** Takes a list of statements, outputs LLVM IR code *)
  let rec generate_llvm_ir (prog: S.prog): L.llvalue list =
    (* Main function here *)
    match prog with
      | Prog ls -> List.map ls ~f:(fun el -> parse_decl el None)
  
  let print_module_to_file (to_file: string) : unit =
    L.print_module to_file this_module

  let string_of_file : string =
    L.string_of_llmodule this_module

end
