[@@@ocaml.warning "-39-32-27-69"]
open Core
module L = Llvm
module S = Syntax_node
module F = Stack_frame.StackFrame

module DefinedFunc = struct
  type t = {
    fn: L.llvalue;
    fntp: L.lltype;
    returns: L.llvalue option;
    returns_to: L.llbasicblock;
  }
end

module TranslateFile = 
struct
  let context = L.global_context()
  let builder = L.builder context
  let this_module = L.create_module context "cocaml_mod"

  let htbl_functions: (S.ident, DefinedFunc.t) Hashtbl.t = Hashtbl.Poly.create ()
  let htbl_types: (S.ident, L.lltype) Hashtbl.t = Hashtbl.Poly.create ()
  let htbl_structs: (S.ident, (S.ident * S.vartype) list) Hashtbl.t = Hashtbl.Poly.create ()

  let var_env = F.create()

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
      begin
        try
          (Hashtbl.find_exn htbl_types custom)
        with Not_found_s _ ->
          failwith @@ "Translation Error: Undefined type: " ^ (ident_to_string custom)
      end
    | Struct str -> Hashtbl.find_exn htbl_types str

  let rec expr_to_vartype (expr: S.expr): S.vartype =
    match expr with
    | IntLiteral _ -> S.Int
    | FloatLiteral _ -> S.Float
    | CharLiteral _ -> S.Char
    | StringLiteral _ -> S.Pointer S.Char
    | MemberAccess (expr, ident, _) -> begin
      let base_type = expr_to_vartype expr in
      (match base_type with
      | Struct s_id ->
        let fields = Hashtbl.find_exn htbl_structs s_id in
        (match List.find fields ~f:(fun (fid, _) -> S.compare_ident fid ident = 0) with
          | Some (_, ft) -> ft
          | None -> failwith "Translation Error: Unknown struct field")
      | _ -> failwith "Translation Error: MemberAccess on non-struct type")
    end
    | PointerMemberAccess (e, field_id, _) -> begin
        match expr_to_vartype e with
        | Pointer (Struct s_id) -> begin
          let fields = Hashtbl.find htbl_structs s_id in
          match fields with
          | Some f ->
            begin
              match List.find f ~f:(fun (fid, _) -> S.compare_ident fid field_id = 0) with
              | Some (_, ft) -> ft
              | None -> failwith "Translation Error: Attempted to access an unknown struct field"
            end
          | None -> failwith "Translation Error: Attempted to access a pointer of a member of a non-existent field."
        end
        | _ -> failwith "Translation Error: Attempted to access a pointer of a member on a non-pointer type."
      end
    | ArrayAccess (expr, _, _) -> begin
      match expr_to_vartype expr with
      | Pointer subtp -> subtp
      | _ -> failwith "Translation Error: Array access on non-pointer type"
      end
    | Var (ident, _) -> begin
      try (F.lookup_variable var_env ident).tp
        with Not_found_s _ -> failwith @@ "Translation Error: Cannot infer type of " ^ (ident_to_string ident)
      end
    | BinOp (bin_op, expr1, expr2, _) -> S.Int
    | Assign (_, expr, _) -> expr_to_vartype expr
    | Call (expr, param, _) -> begin
        match Hashtbl.find htbl_functions expr with
        | Some s -> failwith "TODO"
        | None -> failwith "Translation Error: Cannot infer return type of function"
      end
    | PrefixUnOp (prefix_un_op, expr, _) -> 
      begin
        let t = expr_to_vartype expr in
        match prefix_un_op with
        | Positive | Negative -> t
        | LogicalNot -> S.Int
        | BitwiseNot -> S.Int
        | Address -> S.Pointer t
        | Dereference -> begin
            match t with
            | Pointer subt -> subt
            | _ -> failwith "Translation Error: Attempted to dereference a non-pointer"
          end
        | PrefixIncrement | PrefixDecrement -> t
      end
    | PostfixUnOp (expr, postfix_un_op, _) -> expr_to_vartype expr

  let extract_expr_value (expr: S.expr): L.llvalue =
    match expr with
    | IntLiteral (i, _) -> L.const_int (L.i32_type context) i
    | FloatLiteral (f, _) -> L.const_float (L.float_type context) f
    | CharLiteral (c, _) -> L.const_int (L.i8_type context) (int_of_char c)
    | Var (id, _) -> 
      begin
        try (F.lookup_variable var_env id).value
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
    F.enter_block var_env;
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
      F.declare_variable var_env id { tp = tp; ltp = vartype_to_llvartype tp; value = ll_param_alloc }
    );
    parse_stmt body fn |> ignore;
    F.exit_block var_env;
    Hashtbl.set htbl_functions ~key:label ~data:{ fn = fn; fntp = returns; returns = None; returns_to = entry_block };
    fn

  and parse_expr (expr: S.expr) (scoped_fn: L.llvalue): L.llvalue =
    match expr with
    | IntLiteral (i, _) -> L.const_int ll_int_t i
    | FloatLiteral (f, _) -> L.const_float ll_float_t f
    | CharLiteral (c, _) -> L.const_int ll_char_t (int_of_char c)
    | StringLiteral (s, _) -> failwith "TODO"
    | MemberAccess (m, id, _) -> failwith "TODO"
    | PointerMemberAccess (pt, id, _) -> failwith "TODO"
    | ArrayAccess (ex1, ex2, _) -> failwith "TODO"
    | Var (v, _) ->
      let ll_v = (F.lookup_variable var_env v) in
      L.build_load (ll_v.ltp) (ll_v.value) (ident_to_string v) builder
    | BinOp (bin_op, expr1, expr2, _) -> begin
        let lhs = parse_expr expr1 scoped_fn in
        let rhs = parse_expr expr2 scoped_fn in
        match bin_op with
        | Plus -> L.build_add lhs rhs "add_instr" builder
        | Minus -> L.build_sub lhs rhs "sub_instr" builder
        | Times -> L.build_mul lhs rhs "mul_instr" builder
        | Divide -> L.build_sdiv lhs rhs "div_instr" builder
        | Modulo -> L.build_srem lhs rhs "mod_instr" builder
        | Equal -> L.build_icmp L.Icmp.Eq lhs rhs "eq_icmp" builder
        | NotEqual -> L.build_icmp L.Icmp.Ne lhs rhs "neq_icmp" builder
        | Less -> L.build_icmp L.Icmp.Slt lhs rhs "lt_icmp" builder
        | LessEqual -> L.build_icmp L.Icmp.Sle lhs rhs "leq_icmp" builder
        | Greater -> L.build_icmp L.Icmp.Sgt lhs rhs "gt_icmp" builder
        | GreaterEqual -> L.build_icmp L.Icmp.Sge lhs rhs "gte_icmp" builder
        | BitwiseAnd -> L.build_and lhs rhs "and_bit" builder
        | BitwiseOr -> L.build_or lhs rhs "or_bit" builder
        | BitwiseXor -> L.build_xor lhs rhs "xor_bit" builder
        | LogicalAnd -> L.build_and lhs rhs "and_instr" builder
        | LogicalOr -> L.build_or lhs rhs "or_instr" builder
        | LeftShift -> L.build_shl lhs rhs "shl_instr" builder
        | RightShift -> L.build_ashr lhs rhs "shr_instr" builder
        | PlusAssign -> failwith "TODO"
        | MinusAssign -> failwith "TODO"
        | TimesAssign -> failwith "TODO"
        | DivideAssign -> failwith "TODO"
        | ModuloAssign -> failwith "TODO"
      end
    | Assign (id, e, _) -> 
        let ll_v = (F.lookup_variable var_env id) in
        L.build_store ll_v.value (parse_expr e scoped_fn) builder
    | Call (id, exprs, _) -> begin
        let args = exprs
        |> List.map ~f:(fun el -> parse_expr el scoped_fn)
        |> Array.of_list in
        match Hashtbl.find htbl_functions id with
        | Some fn -> L.build_call fn.fntp fn.fn args "fun_call" builder
        (* TODO: Forward declaration? *)
        | None -> failwith @@ "Cannot find function call to function: " ^ (ident_to_string id)
      end
    | PrefixUnOp (prefix_un_op, e, _) -> failwith "TODO"
    | PostfixUnOp (e, postfix_un_op, _) -> failwith "TODO"

  and parse_stmt (stmt: S.stmt) (scoped_fn: L.llvalue): L.llvalue =
    match stmt with
    | Return (expr, _) -> L.build_ret (parse_expr expr scoped_fn) builder
    | If (expr, then_body, else_body, _) -> begin
        (* Missing: Checking if result evaluates to an integer/bool *)
        let cond_val = parse_expr expr scoped_fn in
        let then_block = L.append_block context "if.then" scoped_fn in
        let else_block = begin
          match else_body with
          | Some b -> L.append_block context "if.else" scoped_fn
          | None -> then_block
        end in
        let finally_block = L.append_block context "if.finally" scoped_fn in
        L.build_cond_br cond_val then_block else_block builder |> ignore_llvalue;
        L.position_at_end then_block builder;
        parse_stmt then_body scoped_fn |> ignore_llvalue;
        L.build_br finally_block builder |> ignore_llvalue;
        (match else_body with
        | Some b -> begin
            L.position_at_end else_block builder;
            parse_stmt b scoped_fn |> ignore_llvalue;
            L.build_br finally_block builder |> ignore_llvalue
          end
        | None -> ());
        L.position_at_end finally_block builder;
        nil_return_type
      end
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
    | For (init_expr, cond_expr, incr_expr, stmt_body, _) ->
      parse_expr init_expr scoped_fn |> ignore_llvalue;
      
      let cond_block = L.append_block context "for.cond" scoped_fn in
      let loop_block = L.append_block context "for.loop" scoped_fn in
      let incr_block = L.append_block context "for.incr" scoped_fn in
      let end_block = L.append_block context "for.end" scoped_fn in

      L.build_br cond_block builder |> ignore_llvalue;
      L.position_at_end cond_block builder;
      let cond_val = parse_expr cond_expr scoped_fn in
      L.build_cond_br cond_val loop_block end_block builder |> ignore_llvalue;

      L.position_at_end loop_block builder;
      ignore (parse_stmt stmt_body scoped_fn);
      L.build_br incr_block builder |> ignore_llvalue;

      L.position_at_end incr_block builder;
      ignore (parse_expr incr_expr scoped_fn);
      L.build_br cond_block builder |> ignore_llvalue;

      L.position_at_end end_block builder;
      nil_return_type
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
    | Switch (expr, cases, _) ->
      let switch_val = parse_expr expr scoped_fn in
      let end_block = L.append_block context "switch.end" scoped_fn in
      let switch_inst = L.build_switch switch_val end_block (List.length cases) builder in
      List.iter cases ~f:(fun c ->
        match c with
        | Case (case_expr, stmt_list, _) ->
          let case_block = L.append_block context "switch.case" scoped_fn in
          let case_val = parse_expr case_expr scoped_fn in
          L.add_case switch_inst case_val case_block;
          L.position_at_end case_block builder;
          List.iter stmt_list ~f:(fun s -> ignore (parse_stmt s scoped_fn));
          ()
        | Default (stmt_list, _) ->
          let default_block = L.append_block context "switch.default" scoped_fn in
          L.add_case switch_inst (L.const_int (L.type_of switch_val) 0) default_block; (* Hack: There's no default for LLVM switches natively, we can place a fake val if needed or just use end_block as default *)
          L.position_at_end default_block builder;
          List.iter stmt_list ~f:(fun s -> ignore (parse_stmt s scoped_fn));
      );

      L.position_at_end end_block builder;
      nil_return_type
    | Break _ -> failwith "TODO"
    | Continue _ -> failwith "TODO"
    | DoWhile (stmt, expr, _) ->
      let loop_block = L.append_block context "do.loop" scoped_fn in
      let cond_block = L.append_block context "do.cond" scoped_fn in
      let end_block = L.append_block context "do.end" scoped_fn in

      L.build_br loop_block builder |> ignore_llvalue;

      L.position_at_end loop_block builder;
      ignore (parse_stmt stmt scoped_fn);
      L.build_br cond_block builder |> ignore_llvalue;

      L.position_at_end cond_block builder;
      let cond_val = parse_expr expr scoped_fn in
      L.build_cond_br cond_val loop_block end_block builder |> ignore_llvalue;

      L.position_at_end end_block builder;
      nil_return_type
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
      F.declare_variable var_env ident { tp = vartype; ltp = lltype; value = alloca };
      begin
        match expr with
        | Some expr_ ->
          let value = parse_expr expr_ scoped_fn in
          L.build_store value alloca builder |> ignore_llvalue
        | None -> ()
      end;
      nil_return_type

  let parse_decl (decl: S.decl) (scoped_fn: L.llvalue option): L.llvalue =
    match decl with
    | GlobalVarDecl (is_static, vartype, ident, expr, position) -> begin
        match expr with
        | Some v -> L.declare_global ll_int_t (ident_to_string ident) this_module
        | None -> failwith "Translation Error: Global variable must be initialized"
      end
    | FuncDecl (vartype, ident, params, stmt, _) ->
      declare_function ident (vartype_to_llvartype vartype) params stmt
    | TypedefDecl (from_vartype, to_vartype, position) ->
      begin
        match from_vartype with
        | Pointer _ | Void -> failwith "Translation Error: Cannot typecast pointer or void." 
        | Typedef custom  -> begin
          match Hashtbl.find htbl_types custom with
          | Some tp -> failwith "TODO"
          | None -> failwith "Translation Error: Could not typecast from unknown type to another."
        end
        | v -> failwith "TODO"
      end
    | StructDecl (id, decl_ls, _) ->
      begin
        let struct_name = ident_to_string id in
        let struct_type = L.named_struct_type context struct_name in
        let field_types = begin
          decl_ls
          |> List.map ~f:(fun el ->
              match el with
              | GlobalVarDecl (_, vt, fid, _, _) -> (fid, vt)
              | _ -> failwith "Translation Error: Structs can only have variable declarations"
            )
          end in
        Hashtbl.set htbl_structs ~key:id ~data:field_types;
        Hashtbl.set htbl_types ~key:id ~data:struct_type;
        let field_types_ll = field_types
          |> List.map ~f:(fun (_, vt) -> vartype_to_llvartype vt)
          |> Array.of_list
        in
        L.struct_set_body struct_type field_types_ll false;
        nil_return_type
      end

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
