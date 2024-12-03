
(* Will be filled out as needed *)
type ir_tokens =
	| STUB

(** Convert C tokens to LLVM IR *)
val c_to_ir_token : Syntax_Node.t -> ir_tokens

(** Do the conversion for a whole tree *)
val c_tree_to_ir_token : Syntax_Node.t list -> ir_tokens list


(* To-Do
 * 1) Semantic Analysis Phase
 * 		a. "Scope" Pass
 * 		b. "Type" Pass
 * 2) Translation Phrase
 *)