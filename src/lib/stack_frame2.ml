open Core
module S = Syntax_node
module K = Stack_frame

type t = 
{ tbl : (S.Ident.t, K.DefinedVar.t Store.Ref.t) Hashtbl.t (* just a quick way to get the ref cells for each variable *)
; snapshots : Store.snapshot Stack.t (* remembers all of the previous ref cell assignments *)
; store : Store.t (* is the structure that the snapshots use in order to restore old assignments *)}

let create () : t =
{ tbl = Hashtbl.Poly.create ()
; snapshots = Stack.create ()
; store = Store.create () }

let enter_block ({ snapshots ; store ; _ } : t) : unit =
Stack.push snapshots @@ Store.capture store

let exit_block ({ snapshots ; store ; _ } : t) : unit =
match Stack.pop snapshots with
| Some snapshot -> Store.restore store snapshot
| None -> failwith "bad exit block"

let declare_variable ({ store ; tbl ; _ } : t) (ident : S.Ident.t) (var_info : K.DefinedVar.t) : unit =
match Hashtbl.find tbl ident with
| Some var_ref ->
  Store.Ref.set store var_ref var_info
| None ->
  let new_ref = Store.Ref.make store var_info in
  Hashtbl.set tbl ~key:ident ~data:new_ref

let lookup_variable ({ tbl ; store ; _ } : t) (ident : S.Ident.t) : K.DefinedVar.t =
match Hashtbl.find tbl ident with
| Some var_ref -> Store.Ref.get store var_ref
| None -> failwith "bad lookup variable"

