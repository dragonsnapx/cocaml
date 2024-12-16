[@@@ocaml.warning "-69-32s"]
open Core
module L = Llvm
module S = Syntax_node

module DefinedVar = struct
  type t = {
    tp: S.vartype;
    ltp: L.lltype;
    value: L.llvalue;
  }
end

module StackFrame =
struct
  type t = (S.ident, DefinedVar.t) Hashtbl.t list ref

  let ident_to_string (ident: S.ident): string =
    match ident with
    | Ident s -> s

  let create(): t =
    ref []

  let enter_block (env: t) : unit =
    env := (Hashtbl.Poly.create()) :: !env
  let exit_block (env: t) : unit =
    match !env with
    | [] -> failwith "Translation Error: (Stack) No scope to exit; Stack underflowed"
    | _ :: rest -> env := rest

  let declare_variable (env: t) (ident: S.ident) (var_info: DefinedVar.t) : unit =
    match !env with
    | [] -> failwith "Translation Error: (Stack) No active scope to declare a variable"
    | scope :: _ -> Hashtbl.set scope ~key:ident ~data:var_info

  (* Lookup a variable, searching from the current scope outward *)
  let lookup_variable (env: t) (ident: S.ident) : DefinedVar.t =
    let rec aux scopes =
      match scopes with
      | [] -> failwith ("Translation Error: (Stack) Unknown identifier: " ^ (ident_to_string ident))
      | scope :: rest ->
        match Hashtbl.find scope ident with
        | Some var_info -> var_info
        | None -> aux rest
    in
    aux !env
end