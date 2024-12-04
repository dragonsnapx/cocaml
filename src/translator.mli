
(* Will be filled out as needed *)
type ir_tokens =
	| STUB

module StringMap = Map.Make(String)

type symbol_entry = {
	name : string;
	pos : Syntax_Node.position;
	typ : string; (* For type checking, e.g., "int", "float", etc. *)
}
	
type symbol_table = symbol_entry StringMap.t
	  
(** Exception raised during semantic analysis *)
exception SemanticError of string * Syntax_Node.position
	  
(** Perform a "scope" pass on a declaration or statement *)
val scope_pass : SymbolTable.t -> Syntax_Node.t -> unit
	  
(** Perform a "type" pass on a declaration or statement *)
val type_pass : SymbolTable.t -> Syntax_Node.t -> unit
	  
(** Translate a single AST node to intermediate representation *)
val c_to_ir_token : Syntax_Node.t -> ir_tokens
	  
(** Translate an entire AST tree to intermediate representation *)
 val c_tree_to_ir_token : Syntax_Node.t list -> ir_tokens list


(* To-Do
 * 1) Semantic Analysis Phase
 * 		a. "Scope" Pass
 * 		b. "Type" Pass
 * 2) Translation Phrase
 *)