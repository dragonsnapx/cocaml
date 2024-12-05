[@@@ocaml.warning "-39-32-27"]
module L = Llvm
module S = Syntax_node

module TranslateFile = 
struct
  let context = L.global_context()
  let builder = L.builder context
  let this_module = L.create_module context "module_name_here"

  let rec transform_variable (tp: S.vartype): L.lltype =
    match tp with
    | Int -> L.i32_type context
    | Float -> L.float_type context
    | Double -> L.double_type context
    | Void -> L.void_type context
    | Pointer subtp -> L.pointer_type context


  let declare_function (label: string) (returns: L.lltype) (formals: L.lltype array) =
    let func_type = L.function_type returns formals in
      match L.lookup_function label this_module with
      | None -> L.declare_function label func_type this_module
      | Some _ -> failwith "Translation Error: Function already exists"
      
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
