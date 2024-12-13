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
    | Custom custom -> 
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
    | UnOp (un_op, expr, _) -> failwith "TODO"

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


  let declare_function (label: S.ident) (returns: L.lltype) (formals: L.lltype array) =
    let func_type = L.function_type returns formals in
      match L.lookup_function (ident_to_string label) this_module with
      | None -> L.declare_function (ident_to_string label) func_type this_module
      | Some _ -> failwith "Translation Error: Function already exists"

  let rec parse_expr (expr: S.expr) (scoped_fn: L.llvalue): L.llvalue =
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
    | UnOp (un_op, e, _) -> failwith "TODO"

  let rec parse_stmt (stmt: S.stmt) (scoped_fn: L.llvalue): L.llvalue =
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
      List.iter stmt_body_ls ~f:(fun el -> parse_stmt el scoped_fn |> ignore_llvalue);
      (* Block has null return by itself *)
      L.const_null (L.void_type context)
    | Switch (expr, cases, _) -> failwith "TODO"
    | Break _ -> failwith "TODO"
    | Continue _ -> failwith "TODO"
    | DoWhile (stmt, expr, _) -> failwith "TODO"

  let parse_decl (decl: S.decl) (scoped_fn: L.llvalue option): L.llvalue =
    (* Local Variable *)
    match decl, scoped_fn with
    | LocalVarDecl (vartype, ident, expr, position), Some fn ->
      begin
        let lltype = vartype_to_llvartype vartype in
        let llname = ident_to_string ident in
        let alloca = L.build_alloca lltype llname builder in
        match expr with
        | Some value -> begin
          let value_type = expr_to_vartype value in
          if (S.compare_vartype value_type vartype) = 0 then
            begin
              Hashtbl.set variables ~key:ident ~data:{ DefinedVar.value = alloca; ltp = lltype; tp = vartype };
              let parsed_val = parse_expr value fn in
              L.build_store parsed_val alloca builder
            end
          else 
            failwith @@ "Translation Error: Cannot assign value to variable " ^ llname
          end
  
        (* No variable initialization*)
        | None -> alloca
      end

    (* Global variable *)
    | GlobalVarDecl (vartype, ident, expr, position), None -> begin
        match expr with
        | Some v -> L.declare_global ll_int_t (ident_to_string ident) this_module
        | None -> failwith "Translation Error: Global variable must be initialized"
      end
    | FuncDecl (vartype, ident, params, stmt, _), _ ->
      params
      |> List.map ~f:(fun el -> 
          el |> fst |> vartype_to_llvartype
      )
      |> Array.of_list
      |> declare_function ident (vartype_to_llvartype vartype)
    | Typedef (from_vartype, to_vartype, position), _ ->
      begin
        match from_vartype with
        | Pointer _ | Void -> failwith "Translation Error: Cannot typecast pointer or void." 
        | Custom custom  -> failwith "TODO"
        | v -> failwith "TODO"
      end
    | StructDecl (_, _, _), _ -> failwith "TODO"

  (** Takes a list of statements, outputs LLVM IR code *)
  let rec generate_llvm_ir (prog: S.prog): L.llvalue list =
    (* Main function here *)
    match prog with
      | Prog ls -> List.map ls ~f:(fun el -> parse_decl el None)
  
  let print_module_to_file (to_file: string) : unit =
    L.print_module to_file this_module

end
