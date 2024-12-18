open Core
module L = Llvm
module S = Syntax_node

module DefinedVar : 
sig
    type t = {
        tp: S.VarType.t;
        ltp: L.lltype;
        value: L.llvalue;
    }
end

module StackFrame :
    sig
        type t = (S.Ident.t, DefinedVar.t) Hashtbl.t list ref
                
        val create : unit -> t
        
        val enter_block : t -> unit

        val exit_block : t -> unit

        val declare_variable : t -> S.Ident.t -> DefinedVar.t -> unit

        val lookup_variable : t -> S.Ident.t -> DefinedVar.t
    end