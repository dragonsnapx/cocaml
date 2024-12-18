[@@@ocaml.warning "-39-32-27-69"]
open Core
module L = Llvm
module S = Syntax_node
module F = Stack_frame.StackFrame

module DefinedFunc = struct
  type t = {
    fn: L.llvalue;
    fnvtp: S.VarType.t;
    name: S.Ident.t;
    returns: L.lltype;
    returns_to: L.llbasicblock;
  }
end

module DefaultParameter : Cocaml_llvm.ModuleParameter = struct
  let module_name = "example_module"
end

module TranslateFile = 
struct
  open Cocaml_math
  module C = Cocaml_llvm.Make(DefaultParameter)

  let htbl_functions: (S.Ident.t, DefinedFunc.t) Hashtbl.t = Hashtbl.Poly.create ()
  let htbl_types: (S.Ident.t, L.lltype) Hashtbl.t = Hashtbl.Poly.create ()
  let htbl_structs: (S.Ident.t, (S.Ident.t * S.VarType.t) list) Hashtbl.t = Hashtbl.Poly.create ()

  let nil_return_type = L.const_null (L.i32_type C.context)

  let ident_to_string (ident: S.Ident.t): string =
    match ident with
    | Ident s -> s

  let rec llvartype_of_vartype (tp: S.VarType.t): L.lltype =
    match tp with
    | Int -> C.ll_int_t
    | Float -> C.ll_float_t
    | Double -> C.ll_double_t
    | Void -> C.ll_void_t
    | Pointer subtp -> C.ll_gen_ptr_t
    | Char -> C.ll_char_t
    | Long -> C.ll_long_t
    | Typedef custom -> 
      begin
        try
          (Hashtbl.find_exn htbl_types custom)
        with Not_found_s _ ->
          C.raise_transl_err @@ "Translation Error: Undefined type: " ^ (ident_to_string custom)
      end
    | Struct str -> Hashtbl.find_exn htbl_types str
    | Array (t, sz) -> C.ll_gen_ptr_t

  let rec expr_to_vartype (expr: S.Expr.t): S.VarType.t =
    match expr with
    | IntLiteral _ -> S.VarType.Int
    | FloatLiteral _ -> S.VarType.Float
    | CharLiteral _ -> S.VarType.Char
    | LongLiteral _ -> S.VarType.Long
    | StringLiteral _ -> S.VarType.Pointer S.VarType.Char
    | MemberAccess (obj, accessor, _) -> begin
      let base_type = expr_to_vartype obj in
      (match base_type with
      | Struct s_id ->
        let fields = Hashtbl.find_exn htbl_structs s_id in
        (match List.find fields ~f:(fun (fid, _) -> S.Ident.compare fid accessor = 0) with
          | Some (_, ft) -> ft
          | None -> C.raise_transl_err "Unknown struct field")
      | _ -> C.raise_transl_err "MemberAccess on non-struct type")
    end
    | PointerMemberAccess (obj, child_id, _) -> begin
        match expr_to_vartype obj with
        | Pointer (Struct s_id) -> begin
          let fields = Hashtbl.find htbl_structs s_id in
          match fields with
          | Some f ->
            begin
              match List.find f ~f:(fun (fid, _) -> S.Ident.compare fid child_id = 0) with
              | Some (_, ft) -> ft
              | None -> C.raise_transl_err "Attempted to access an unknown struct field"
            end
          | None -> C.raise_transl_err "Attempted to access a pointer of a member of a non-existent field."
        end
        | _ -> C.raise_transl_err "Attempted to access a pointer of a member on a non-pointer type."
      end
    | ArrayAccess (expr, _, _) -> begin
      match expr_to_vartype expr with
      | Pointer subtp -> subtp
      | _ -> C.raise_transl_err "Array access on non-pointer type"
      end
    | Var (ident, _) -> begin
      try (F.lookup_variable C.var_env ident).tp
        with Not_found_s _ -> C.raise_transl_err @@ " Cannot infer type of " ^ (ident_to_string ident)
      end
    | BinOp (bin_op, expr1, expr2, _) -> S.VarType.Int
    | Assign (_, expr, _) -> expr_to_vartype expr
    | Call (expr, param, _) -> begin
        match Hashtbl.find htbl_functions expr with
        | Some s -> s.fnvtp
        | None -> C.raise_transl_err "Cannot infer return type of function"
      end
    | PrefixUnOp (prefix_un_op, expr, _) -> 
      begin
        let t = expr_to_vartype expr in
        match prefix_un_op with
        | Positive | Negative -> t
        | LogicalNot -> S.VarType.Int
        | BitwiseNot -> S.VarType.Int
        | Address -> S.VarType.Pointer t
        | Dereference -> begin
            match t with
            | Pointer subt -> subt
            | _ -> C.raise_transl_err "Attempted to dereference a non-pointer"
          end
        | PrefixIncrement | PrefixDecrement -> t
      end
    | PostfixUnOp (expr, postfix_un_op, _) -> expr_to_vartype expr

  let extract_expr_value (expr: S.Expr.t): L.llvalue =
    match expr with
    | IntLiteral (i, _) -> C.const_ll_int_t i
    | FloatLiteral (f, _) -> C.const_ll_float_t f
    | CharLiteral (c, _) -> C.const_ll_char_t (int_of_char c)
    | LongLiteral (l, _) -> C.const_ll_long_t l
    | Var (id, _) -> 
      begin
        try (F.lookup_variable C.var_env id).value
        with Not_found_s _  -> C.raise_transl_err @@ "Cannot extract value from " ^ (ident_to_string id)
      end
    | _ -> C.raise_transl_err "extract_expr_value must be called on literal"

  let func_entry_builder (): L.llbuilder =
    L.insertion_block C.builder
    |> L.block_parent
    |> L.entry_block
    |> L.instr_begin
    |> L.builder_at C.context


  let rec declare_function (label: S.Ident.t) (returns: S.VarType.t) (params: (S.VarType.t * S.Ident.t) list) (body: S.Stmt.t) =
    F.enter_block C.var_env;
    let params_ll_value = 
      List.map params ~f:(fun elt -> elt |> fst |> llvartype_of_vartype) 
      |> Array.of_list 
    in
    let func_type = L.function_type (llvartype_of_vartype returns) params_ll_value in
    let fn_name = ident_to_string label in
    let fn = match L.lookup_function fn_name C.this_module with
    | None -> L.declare_function fn_name func_type C.this_module
    | Some _ -> C.raise_transl_err "Function already exists" in
    let entry_block = C.append_block "entry" fn in
    C.position_at_end entry_block;
    List.iteri params ~f:(fun i (tp, id) -> 
      let ll_param = L.param fn i in
      L.set_value_name (ident_to_string id) ll_param;
      let ll_param_alloc = L.build_alloca (llvartype_of_vartype tp) (ident_to_string id) C.builder in
      L.build_store ll_param ll_param_alloc C.builder |> C.ignore_llvalue;
      F.declare_variable C.var_env id { tp = tp; ltp = llvartype_of_vartype tp; value = ll_param_alloc }
    );
    parse_stmt body fn |> ignore;
    F.exit_block C.var_env;
    Hashtbl.set htbl_functions ~key:label ~data:{ name = label; fn = fn; fnvtp = returns; returns = func_type; returns_to = entry_block };
    fn

  and parse_expr (expr: S.Expr.t) (scoped_fn: L.llvalue): L.llvalue =
    let parse_expr expr = parse_expr expr scoped_fn in
    match expr with
    | IntLiteral (i, _) -> C.const_ll_int_t i
    | FloatLiteral (f, _) -> C.const_ll_float_t f
    | CharLiteral (c, _) -> C.const_ll_char_t (int_of_char c)
    | LongLiteral (l, _) -> C.const_ll_long_t l
    | StringLiteral (s, _) -> failwith "TODO"
    | MemberAccess (str, id, _) ->
      let struct_val = parse_expr str in
      let struct_type = expr_to_vartype str in
      let s_id =
        match struct_type with
        | Struct sid -> sid
        | _ -> C.raise_transl_err ("MemberAccess on a non-struct type: " ^ (ident_to_string id))
      in

      let fields = 
        match Hashtbl.find htbl_structs s_id with
        | Some fs -> fs
        | None -> C.raise_transl_err ("Cannot access field for struct: " ^ (ident_to_string s_id))
      in

      let (field_index, (_, field_tp)) =
        match List.findi fields ~f:(fun _ (fid, _) -> S.Ident.compare fid id = 0) with
        | Some (i, pair) -> (i, pair)
        | None -> C.raise_transl_err ("Translation Error: Unknown field " ^ (ident_to_string id))
      in

      let ll_struct_type = llvartype_of_vartype struct_type in
      let tmp_alloca = L.build_alloca ll_struct_type "tmp_struct" C.builder in
      L.build_store struct_val tmp_alloca C.builder |> ignore;

      let field_ptr = L.build_struct_gep ll_struct_type tmp_alloca field_index "field_ptr" C.builder in
      L.build_load (llvartype_of_vartype field_tp) field_ptr "field_val" C.builder
    | PointerMemberAccess (ptr, id, _) -> failwith "TODO"
    | ArrayAccess (arr, acc, _) ->  begin
      let arr_val = parse_expr arr in
      let index_val = parse_expr acc in
      let arr_type = expr_to_vartype arr in
      let ll_arr_type = llvartype_of_vartype arr_type in
      match arr_type with
      (* int* arr *)
      | Pointer subtp ->
        let element_ptr = L.build_gep ll_arr_type arr_val [| index_val |] "element_ptr" C.builder in
        L.build_load (llvartype_of_vartype subtp) element_ptr "element_val" C.builder
      (* int[] arr *)
      | _ ->
        let zero = C.const_ll_int_t 0 in
        let element_ptr = L.build_gep ll_arr_type arr_val [| zero; index_val |] "element_ptr" C.builder in
        let subtp =
          match arr_type with
          | Pointer stp -> stp
          | _ -> C.raise_transl_err "Failed to infer type of array"
        in
        L.build_load (llvartype_of_vartype subtp) element_ptr "element_val" C.builder
    end

    | Var (v, _) ->
      let ll_v = (F.lookup_variable C.var_env v) in
      L.build_load (ll_v.ltp) (ll_v.value) (ident_to_string v) C.builder
    | BinOp (bin_op, expr1, expr2, _) ->
      let lhs = parse_expr expr1 in
      let rhs = parse_expr expr2 in
      process_binop bin_op lhs rhs C.builder
    | Assign (to_var, from_var, _) -> 
        let from_val = parse_expr from_var in
        let to_ptr = ptr_of to_var in
        L.build_store from_val (to_ptr scoped_fn) C.builder |> ignore;
        from_val
    | Call (id, exprs, _) -> begin
        let args = exprs
        |> List.map ~f:(fun el -> parse_expr el)
        |> Array.of_list in
        match Hashtbl.find htbl_functions id with
        | Some fn ->
            let fn_call_name = "fn_call_" ^ (ident_to_string fn.name) in
            L.build_call fn.returns fn.fn args fn_call_name  C.builder
        | None -> C.raise_transl_err @@ "Cannot find function call to function: " ^ (ident_to_string id)
      end
    | PrefixUnOp (prefix_un_op, var, _) -> begin
      let var_type = expr_to_vartype var in
      let var_lltype = llvartype_of_vartype var_type in
      let var_expr = parse_expr var in
      match prefix_un_op with
      | Positive -> var_expr
      | Negative -> L.build_neg var_expr "neg_val" C.builder
      | LogicalNot -> 
          let zero = L.const_int (L.type_of var_expr) 0 in
          L.build_icmp L.Icmp.Eq var_expr zero "log_not_val" C.builder
      | BitwiseNot ->
        L.build_not var_expr "bitwise_not_val" C.builder
      | Address -> ptr_of var scoped_fn
      | Dereference ->
          let subtp =
            match var_type with
            | Pointer t -> t
            | _ ->  C.raise_transl_err "Dereference of a non-pointer"
          in
          L.build_load (llvartype_of_vartype subtp) var_expr "deref_val" C.builder
      (* TODO: Dedupe code *)
      | PrefixIncrement ->
          let ptr = ptr_of var scoped_fn in
          let old_val = L.build_load var_lltype ptr "old_val" C.builder in
          let one = L.const_int var_lltype 1 in
          let new_val = L.build_add old_val one "inc_val" C.builder in
          ignore (L.build_store new_val ptr C.builder);
          new_val
      | PrefixDecrement ->
          let ptr = ptr_of var scoped_fn in
          let old_val = L.build_load var_lltype ptr "old_val" C.builder in
          let one = L.const_int var_lltype 1 in
          let new_val = L.build_sub old_val one "dec_val" C.builder in
          ignore (L.build_store new_val ptr C.builder);
          new_val
      end
    | PostfixUnOp (ex, postfix_un_op, _) -> 
      match postfix_un_op with
      | _ -> failwith "OK"

  and parse_stmt (stmt: S.Stmt.t) (scoped_fn: L.llvalue): L.llvalue =
    let parse_expr stmt = parse_expr stmt scoped_fn in
    match stmt with
    | Return (expr, _) -> L.build_ret (parse_expr expr) C.builder
    | If (expr, then_body, else_body, _) -> begin
        (* Missing: Checking if result evaluates to an integer/bool *)
        let cond_val = parse_expr expr in
        let then_block = C.append_block "if.then" scoped_fn in
        let else_block = begin
          match else_body with
          | Some b -> C.append_block "if.else" scoped_fn
          | None -> then_block
        end in
        let finally_block = C.append_block "if.finally" scoped_fn in
        L.build_cond_br cond_val then_block else_block C.builder |> C.ignore_llvalue;
        L.position_at_end then_block C.builder;
        parse_stmt then_body scoped_fn |> C.ignore_llvalue;
        L.build_br finally_block C.builder |> C.ignore_llvalue;
        (match else_body with
        | Some b -> begin
            L.position_at_end else_block C.builder;
            parse_stmt b scoped_fn |> C.ignore_llvalue;
            L.build_br finally_block C.builder |> C.ignore_llvalue
          end
        | None -> ());
        L.position_at_end finally_block C.builder;
        nil_return_type
      end
    | While (expr, stmt, _) -> begin
      let condition_block = C.append_block "while.cond" scoped_fn in
      let condition = parse_expr expr in
      let while_block = C.append_block "while.true" scoped_fn in
      let end_block = C.append_block "while.end" scoped_fn in
      L.position_at_end condition_block C.builder;
      let break_end = L.build_cond_br condition while_block end_block C.builder in
      L.position_at_end while_block C.builder;
      parse_stmt stmt scoped_fn |> C.ignore_llvalue;
      L.position_at_end end_block C.builder;
      break_end
    end
    | For (init_expr, cond_expr, incr_expr, stmt_body, _) ->
      (match init_expr with
      | ForExpr expr_init ->
          parse_expr expr_init |> C.ignore_llvalue
      | ForVarDecl S.Var_decl (is_static, var_type, ident, init_opt, _) ->
          let ll_var_type = llvartype_of_vartype var_type in
          let ll_var_alloc = L.build_alloca ll_var_type (ident_to_string ident) C.builder in
    
          (match init_opt with
            | Some init_expr ->
                let init_val = parse_expr init_expr in
                L.build_store init_val ll_var_alloc C.builder |> C.ignore_llvalue
            | None -> ());
    
          F.declare_variable C.var_env ident {tp = var_type; ltp = ll_var_type; value = ll_var_alloc; }); 

      let cond_block = C.append_block "for.cond" scoped_fn in
      let loop_block = C.append_block "for.loop" scoped_fn in
      let incr_block = C.append_block "for.incr" scoped_fn in
      let end_block = C.append_block "for.end" scoped_fn in

      L.build_br cond_block C.builder |> C.ignore_llvalue;
      L.position_at_end cond_block C.builder;
      let cond_val = parse_expr cond_expr in
      L.build_cond_br cond_val loop_block end_block C.builder |> C.ignore_llvalue;

      C.position_at_end loop_block;
      ignore (parse_stmt stmt_body scoped_fn);
      L.build_br incr_block C.builder |> C.ignore_llvalue;

      C.position_at_end incr_block;
      ignore (parse_expr incr_expr);
      L.build_br cond_block C.builder |> C.ignore_llvalue;

      C.position_at_end end_block;
      nil_return_type
    | ExprStmt (expr, _) -> parse_expr expr
    | Block (stmt_body_ls, _) -> 
      let current_block = L.insertion_block C.builder in
      let scoped_block =
        List.fold_left stmt_body_ls ~init:current_block ~f:(fun _ stmt ->
          parse_stmt stmt scoped_fn |> C.ignore_llvalue;
          L.insertion_block C.builder
        )
      in
      C.position_at_end scoped_block;
      nil_return_type
    | Switch (expr, cases, _) ->
      let switch_val = parse_expr expr in
      let end_block = C.append_block "switch.end" scoped_fn in
      let switch_inst = L.build_switch switch_val end_block (List.length cases) C.builder in
      List.iter cases ~f:(fun c ->
        match c with
        | Case (case_expr, stmt_list, _) ->
          let case_block = C.append_block "switch.case" scoped_fn in
          let case_val = parse_expr case_expr in
          L.add_case switch_inst case_val case_block;
          C.position_at_end case_block;
          List.iter stmt_list ~f:(fun s -> ignore (parse_stmt s scoped_fn));
          ()
        | Default (stmt_list, _) ->
          let default_block = C.append_block "switch.default" scoped_fn in
          L.add_case switch_inst (L.const_int (L.type_of switch_val) 0) default_block; (* Hack: There's no default for LLVM switches natively, we can place a fake val if needed or just use end_block as default *)
          C.position_at_end default_block;
          List.iter stmt_list ~f:(fun s -> ignore (parse_stmt s scoped_fn));
      );
      C.position_at_end end_block;
      nil_return_type
    | Break _ -> C.raise_transl_err "Break is unsupported"
    | Continue _ -> C.raise_transl_err "Continue is unsupported"
    | DoWhile (stmt, expr, _) ->
      let loop_block = C.append_block "do.loop" scoped_fn in
      let cond_block = C.append_block "do.cond" scoped_fn in
      let end_block = C.append_block "do.end" scoped_fn in

      L.build_br loop_block C.builder |> C.ignore_llvalue;

      C.position_at_end loop_block;
      ignore (parse_stmt stmt scoped_fn);
      L.build_br cond_block C.builder |> C.ignore_llvalue;

      C.position_at_end cond_block;
      let cond_val = parse_expr expr in
      L.build_cond_br cond_val loop_block end_block C.builder |> C.ignore_llvalue;

      C.position_at_end end_block;
      nil_return_type
    | VarDecl S.Var_decl (is_static, vartype, ident, expr, _) ->
      let lltype = llvartype_of_vartype vartype in
      let llname = ident_to_string ident in
      let alloca = 
        L.insertion_block C.builder 
        |> L.block_parent 
        |> L.entry_block
        |> L.instr_begin
        |> L.builder_at C.context
        |> L.build_alloca lltype llname
      in
      F.declare_variable C.var_env ident { tp = vartype; ltp = lltype; value = alloca };
      begin
        match expr with
        | Some expr_ ->
          let value = parse_expr expr_ in
          L.build_store value alloca C.builder |> C.ignore_llvalue
        | None -> ()
      end;
      nil_return_type
    | StructDecl s -> declare_struct(s)
    | StructInit s -> initialize_struct(s, scoped_fn)
    | TypedefDecl S.Typedef_decl (var, id, _) -> failwith "OK"

  and parse_decl (decl: S.Decl.t) (scoped_fn: L.llvalue option): L.llvalue =
    match decl with
    | VarDecl S.Var_decl (is_static, vartype, ident, expr, _) -> begin
        match expr with
        | Some v -> L.declare_global C.ll_int_t (ident_to_string ident) C.this_module
        | None -> C.raise_transl_err "Global variable must be initialized"
      end
    | FuncDecl (vartype, ident, params, stmt, _) ->
      declare_function ident vartype params stmt
    | TypedefDecl S.Typedef_decl (from_vartype, to_vartype, _) ->
      begin
        match from_vartype with
        | Pointer _ | Void -> C.raise_transl_err "Cannot typecast pointer or void." 
        | Typedef custom  -> begin
          match Hashtbl.find htbl_types custom with
          | Some tp -> Hashtbl.set htbl_types ~key:custom ~data:tp; nil_return_type
          | None -> C.raise_transl_err "Could not typecast from unknown type to another."
        end
        | v -> failwith "TODO"
      end
    | StructDecl s -> declare_struct(s)
    | StructInit s -> 
      match scoped_fn with
      | Some fn -> initialize_struct(s, fn)
      | None -> C.raise_transl_err "Struct cannot be initialized in a context-less environment"

  and declare_struct (S.Struct_decl (struct_name, var_name, decl_ls_opt, _)) =
    let struct_name_str = ident_to_string struct_name in
    let struct_type = L.named_struct_type C.context struct_name_str in
    let (fields, field_types) =
      match decl_ls_opt with
      | Some decl_ls -> 
        let fields =
          List.map decl_ls ~f:(fun decl ->
            match decl with
            | Var_decl (_, vartype, ident, _, _) -> (ident, vartype)
          )
        in
        let field_types_ll = 
          fields
          |> List.map ~f:(fun (_, vt) -> llvartype_of_vartype vt)
          |> Array.of_list
        in
        (fields, field_types_ll)
      | None -> ([], [||])
    in
    Hashtbl.set htbl_structs ~key:struct_name ~data:fields;
    L.struct_set_body struct_type field_types false;
    Hashtbl.set htbl_types ~key:struct_name ~data:struct_type;
    nil_return_type

  and initialize_struct (S.Struct_init (struct_name, var_name, expr_opt, _), scoped_fn) =
    let struct_type =
      match Hashtbl.find htbl_types struct_name with
      | Some st -> st
      | None -> C.raise_transl_err ("Unknown struct type: " ^ (ident_to_string struct_name))
    in
    let var_name_str = ident_to_string var_name in
    let alloca =
      L.insertion_block C.builder
      |> L.block_parent
      |> L.entry_block
      |> L.instr_begin
      |> L.builder_at C.context
      |> L.build_alloca struct_type var_name_str
    in
    F.declare_variable C.var_env var_name { tp = Struct struct_name; ltp = struct_type; value = alloca };
    match expr_opt with
    | Some expr ->
      let init_val = parse_expr expr scoped_fn in
      let fields =
        match Hashtbl.find htbl_structs struct_name with
        | Some fs -> fs
        | None -> C.raise_transl_err ("Unknown struct fields: " ^ (ident_to_string struct_name))
      in
      List.iteri fields ~f:(fun i (fid, ft) -> 
        let dest_ptr = L.build_struct_gep struct_type alloca i (var_name_str ^ "_field_ptr") C.builder in
        let src_ptr = L.build_struct_gep struct_type init_val i ((ident_to_string var_name) ^ "_init_field_ptr") C.builder in
        let field_val = L.build_load (llvartype_of_vartype ft) src_ptr (ident_to_string fid ^ "_val") C.builder in
        ignore (L.build_store field_val dest_ptr C.builder)
      );
      nil_return_type
    | None -> nil_return_type

  and ptr_of (expr: S.Expr.t) (scoped_fn: L.llvalue) = 
    let ptr_to_struct struct_expr =
      let struct_val = parse_expr struct_expr scoped_fn in
      let struct_type = expr_to_vartype struct_expr in
      let ll_struct_type = llvartype_of_vartype struct_type in
      let tmp_alloca = L.build_alloca ll_struct_type "tmp_struct" C.builder in
      L.build_store struct_val tmp_alloca C.builder |> ignore;
      tmp_alloca
    in
    let find_struct_child struct_expr child =
      let struct_type =
        match expr_to_vartype struct_expr with
        | Struct s_id -> s_id
        | Pointer (Struct s_id) -> s_id
        | _ -> C.raise_transl_err "Cannot access a non-struct as a struct"
      in
      let fields = Hashtbl.find_exn htbl_structs struct_type in
      match List.findi fields ~f:(fun _ (fid, _) -> S.Ident.compare fid child = 0) with
      | Some (i, (_, ftype)) -> (i, llvartype_of_vartype ftype)
      | None -> failwith "Field not found"
    in
    match expr with
    | Var (id, _) -> (F.lookup_variable C.var_env id).value
    | MemberAccess (struct_expr, child_id, _) ->
      let struct_ptr = ptr_to_struct struct_expr in
      let (child_index, child_type) = find_struct_child struct_expr child_id in
      L.build_struct_gep child_type struct_ptr child_index "child_ptr" C.builder
    | PointerMemberAccess (ptr_expr, child_id, _) ->
      let ptr_val = parse_expr ptr_expr scoped_fn in
      let (child_index, child_type) = find_struct_child ptr_expr child_id in
      L.build_struct_gep child_type ptr_val child_index "child_ptr" C.builder
    | ArrayAccess (arr_expr, idx_expr, _) ->
        let arr_val = parse_expr arr_expr scoped_fn in
        let idx_val = parse_expr idx_expr scoped_fn in
        let arr_ll_type = arr_val |> L.type_of |> L.element_type in
        L.build_gep arr_ll_type arr_val [| C.const_llvalue_zero;  idx_val |] "element_ptr" C.builder
    | _ -> C.raise_transl_err "Cannot take address of an rvalue"

  (** Takes a list of statements, outputs LLVM IR code *)
  let rec generate_llvm_ir (prog: S.prog): L.llvalue list =
    (* Main function here *)
    match prog with
      | Prog ls -> List.map ls ~f:(fun el -> parse_decl el None)
  
  let print_module_to_file (to_file: string) : unit =
    L.print_module to_file C.this_module

  let string_of_file : string =
    L.string_of_llmodule C.this_module

end
