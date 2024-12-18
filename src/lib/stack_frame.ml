[@@@ocaml.warning "-69-32"]
open Core
module L = Llvm
module S = Syntax_node

module DefinedVar = struct
  type t = {
    tp: S.VarType.t;
    ltp: L.lltype;
    value: L.llvalue;
  }
end

exception TranslationStackError of string

let raise_transl_st_err msg =
  raise (TranslationStackError ("Translation Error -- STACK: " ^ msg))

type item_t = (S.Ident.t, DefinedVar.t) Hashtbl.t
type t = item_t Stack.t

let ident_to_string (ident: S.Ident.t): string =
  match ident with
  | Ident s -> s

let create(): t =
  Stack.create()

let enter_block (env: t) : unit =
  Stack.push env (Hashtbl.Poly.create())
let exit_block (env: t) : unit =
  match Stack.pop env with
  | None -> raise_transl_st_err "(Stack) No scope to exit; Stack underflowed"
  | Some _ -> ()

let declare_variable (env: t) (ident: S.Ident.t) (var_info: DefinedVar.t) : unit =
  match Stack.top env with
  | None -> raise_transl_st_err "(Stack) No active scope to declare a variable"
  | Some scope -> Hashtbl.set scope ~key:ident ~data:var_info

(* Lookup a variable, searching from the current scope outward *)
let lookup_variable (env: t) (ident: S.Ident.t) : DefinedVar.t =
  let scopes = Stack.to_list env in
  let rec aux = function
    | [] -> raise_transl_st_err ("(Stack) Unknown identifier: " ^ (ident_to_string ident))
    | scope :: rest ->
      match Hashtbl.find scope ident with
      | Some var_info -> var_info
      | None -> aux rest
  in
  aux scopes