(* Plan for the behavior of our parser -- I believe all of this logic will be implemented in menhir_parser.mly *)

(* List of Menhir tokens *)
type keywords_token =
	| AUTO
	| BREAK
	| CASE
	| CHAR of char
	| CONST
	| CONTINUE
	| DEFAULT
	| DO
	| DOUBLE
	| ELSE
	| ENUM
	| EXTERN
	| FLOAT
	| FOR
	| GOTO
	| IF
	| INT of int
	| LONG of int64
	| REGISTER
	| RETURN
	| SHORT
	| SIGNED
	| SIZEOF
	| STATIC
	| STRUCT
	| SWITCH
	| TYPEDEF
	| UNION
	| UNSIGNED
	| VOID
	| VOLATILE
	| WHILE
	| IDENT of string 			(* Do we need these last two? *)
    | EOF

(*
module Syntax_Node : sig
	type t
	val pos_start : int
  val pos_end : int
	val token_type : keywords_token
end
*)

(* Structure of our AST nodes *)
module Syntax_Node : sig
	type position = {
	  pos_start: int; 
	  pos_end: int; 
	}

	val create_position : int -> int -> position
	val with_position : int -> int -> (position -> 'a) -> 'a
  
	(* Expressions: For literals, variables, operations, etc. *)
	type expr =
	  | IntLiteral of int * position         						
	  | FloatLiteral of float * position     						
	  | CharLiteral of char * position       						
	  | Var of string * position             						(* Variable Names *)
	  | BinOp of string * expr * expr * position  					(* Example: a + b *)
	  | Assign of string * expr * position   						(* Example: x = 5 *)
	  | Call of string * expr list * position 						(* Example: foo(1, 2) *)
  
	(* Statements: For control flow, return, expressions, etc. *)
	type stmt =
	  | Return of expr * position
	  | If of expr * stmt * stmt option * position
	  | While of expr * stmt * position
	  | For of expr * expr * expr * stmt * position
	  | ExprStmt of expr * position
	  | Block of stmt list * position
  
	(* Declarations: For vaariables and functions *)
	type decl =
	  | VarDecl of string * expr option * position  				(* Example: int x = 5; *)
	  | FuncDecl of string * string list * stmt * position			(* Example: int main() { return 0; } *)
  
	(* At the top level, every syntax node is either a declaration or a statement *)
	type t =
	  | Decl of decl
	  | Stmt of stmt
end
  

