[@@@ocaml.warning "-39-32-27"]
open Core
module L = Llvm
module S = Syntax_node

module DefinedFunc = struct
  type t = {
    fn: L.llvalue;
    returns: L.llvalue option;
    returns_to: L.llbasicblock;
  }
end

module DefinedVar = struct
  type t = {
    tp: S.vartype;
    value: L.llvalue;
  }
end

module TranslateFile = 
struct
  let context = L.global_context()
  let builder = L.builder context
  let this_module = L.create_module context "module_name_here"

  let functions: (DefinedFunc.t, int) Hashtbl.t = Hashtbl.Poly.create ()
  let mixins = Hashtbl.create

  type types_hashtbl_key = S.ident * L.lltype

  let types: (S.ident, L.lltype) Hashtbl.t = Hashtbl.Poly.create ()
  let variables: (S.ident, DefinedVar.t) Hashtbl.t = Hashtbl.Poly.create ()

  let ignore_llvalue (v : L.llvalue) : unit =
    ()

  let ident_to_string (ident: S.ident): string =
    match ident with
    | Ident s -> s

  let rec vartype_to_llvartype (tp: S.vartype): L.lltype =
    match tp with
    | Int -> L.i32_type context
    | Float -> L.float_type context
    | Double -> L.double_type context
    | Void -> L.void_type context
    | Pointer subtp -> L.pointer_type context
    | Char -> failwith "exception: symbol char not defined"
    | Custom custom -> 
      try
        (Hashtbl.find_exn types custom)
      with Not_found_s _ ->
        failwith @@ "Translation Error: Undefined type: " ^ (ident_to_string custom) 

  let rec expr_to_vartype (expr: S.expr): S.vartype =
    match expr with
    | IntLiteral _ -> S.Int
    | FloatLiteral _ -> S.Float
    | CharLiteral _ -> S.Char
    | Var (ident, _) -> begin
      try (Hashtbl.find_exn variables ident).tp
      with Not_found_s _ -> failwith @@ "Translation Error: Cannot infer type of " ^ (ident_to_string ident)
      end
    | BinOp (bin_op, expr1, expr2, _) -> failwith "TODO"
    | Assign (ident, expr, _) -> failwith "TODO"
    | Call (expr, param, _) -> failwith "TODO"
    | UnOp (un_op, expr, _) -> failwith "TODO"


  let declare_function (label: S.ident) (returns: L.lltype) (formals: L.lltype array) =
    let func_type = L.function_type returns formals in
      match L.lookup_function (ident_to_string label) this_module with
      | None -> L.declare_function (ident_to_string label) func_type this_module
      | Some _ -> failwith "Translation Error: Function already exists"

  let parse_decl (decl: S.decl): unit =
    match decl with
    | VarDecl (vartype, ident, expr, position) ->
      begin
        let lltype = vartype_to_llvartype vartype in
        let llname = ident_to_string ident in
        let alloca = L.build_alloca lltype llname builder in
        match expr with
        | Some value -> begin
          let value_type = expr_to_vartype value in
          if value_type == vartype then
            L.build_store value alloca builder |> ignore_llvalue
          else 
            failwith @@ "Translation Error: Cannot assign value``  to variable " ^ llname
          end
        | None -> ();
        Hashtbl.set variables ~key:ident ~data:{ DefinedVar.value = alloca; tp = vartype }
      end
    | FuncDecl (vartype, ident, params, stmt, _) ->
      params
      |> List.map ~f:(fun el -> 
          el |> fst |> vartype_to_llvartype
        )
      |> Array.of_list
      |> declare_function ident vartype 
      |> ignore_llvalue
    | Typedef (from_vartype, to_vartype, position) ->
      begin
        match from_vartype with
        | Pointer _ | Void -> failwith "Translation Error: Cannot typecast pointer or void." 
        | Custom custom  -> ()
        | v -> ()
      end
    | StructDecl (_, _, _) -> ()

  let parse_stmt (stmt: S.stmt) =
    match stmt with
    | Return (expr, _) -> begin
        
      end
    | If (expr, stmt, stmt_body, _) -> ()
    | While (expr, stmt, _) -> ()
    | For (expr1, expr2, expr3, stmt_body, _) -> ()
    | ExprStmt (expr, _) -> ()
    | Block (stmt_body_ls, _) -> ()
    | Switch (expr, cases, _) -> ()
     

  (** Takes a list of statements, outputs LLVM IR code *)
  let rec expr_parser (statements: S.t list): string list =
    let parse_line (statement : S.t) : string =
      match statement with
      | S.Decl stmt -> ""
      | S.Stmt stmt -> ""
    in
    let rec aux (ls: S.t list) (agg: string list) : string list =
      match ls with
      | [] -> agg
      | x :: xs -> (parse_line x) :: (aux xs agg)
    in
    List.rev @@ aux statements []

end
