type position = {
  pos_start: int; 
  pos_end: int;
}

val create_position : int -> int -> position
val with_position : int -> int -> (position -> 'a) -> 'a

(* Identifier type for variable, function, and user-defined type names *)
type ident = Ident of string [@@deriving compare]

(* Type annotations for variables and function return types *)
type vartype =
  | Int
  | Float
  | Char
  | Long
  | Double
  | Void
  | Custom of ident
  | Pointer of vartype [@@deriving compare]

type bin_op =
  | Plus           
  | Minus          
  | Times         
  | Divide         
  | Modulo         
  | Equal          
  | NotEqual       
  | Less           
  | LessEqual      
  | Greater       
  | GreaterEqual   
  | LogicalAnd     
  | LogicalOr     
  | BitwiseAnd    
  | BitwiseOr     
  | BitwiseXor   
  
type un_op =
  | Positive
  | Negative         
  | LogicalNot   
  | BitwiseNot 
  | Address        
  | Dereference    

type expr =
  | IntLiteral of int * position
  | FloatLiteral of float * position
  | CharLiteral of char * position
  | Var of ident * position
  | BinOp of bin_op * expr * expr * position
  | Assign of ident * expr * position
  | Call of ident * expr list * position
  | UnOp of un_op * expr * position

type stmt =
  | Return of expr * position
  | If of expr * stmt * stmt option * position
  | While of expr * stmt * position
  | For of expr * expr * expr * stmt * position
  | ExprStmt of expr * position
  | Block of stmt list * position
  | Switch of expr * case list * position
  | Break of position
  | Continue of position
  | DoWhile of stmt * expr * position
  
and case =
	| Case of expr * stmt list * position
	| Default of stmt list * position
	
type decl =
  | VarDecl of vartype * ident * expr option * position                       (* Example: int x = 10 *)
  | FuncDecl of vartype * ident * (vartype * ident) list * stmt * position    (* Example: int f(int a, int b) {} *)
  | Typedef of vartype * vartype * position                                   (* Example: typedef int Integer *)
  | StructDecl of ident * decl list * position                                (* Example: Struct Pair {int x; int y};*)

type prog = Prog of decl list

