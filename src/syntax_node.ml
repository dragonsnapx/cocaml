type position = {
  pos_start: int; 
  pos_end: int;
}

let create_position start_pos end_pos = { pos_start = start_pos; pos_end = end_pos }

let with_position start_pos end_pos f =
  let position = create_position start_pos end_pos in
  f position

	type expr =
  | IntLiteral of int * position
  | FloatLiteral of float * position
  | CharLiteral of char * position
  | Var of string * position
  | BinOp of string * expr * expr * position
  | Assign of string * expr * position
  | Call of string * expr list * position
  | UnOp of string * expr * position

type stmt =
  | Return of expr * position
  | If of expr * stmt * stmt option * position
  | While of expr * stmt * position
  | For of expr * expr * expr * stmt * position
  | ExprStmt of expr * position
  | Block of stmt list * position
  | Switch of expr * case list * position

and case =
	| Case of expr * stmt list * position
	| Default of stmt list * position
	
type decl =
  | VarDecl of string * expr option * position
  | FuncDecl of string * string list * stmt * position
  | Typedef of string * string * position
  | StructDecl of string * decl list * position
  
type t =
	| Decl of decl
	| Stmt of stmt
