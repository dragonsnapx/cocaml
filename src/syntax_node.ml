open Core

type position = {
  pos_start: int; 
  pos_end: int;
}

let create_position start_pos end_pos = { pos_start = start_pos; pos_end = end_pos }

let with_position start_pos end_pos f =
  let position = create_position start_pos end_pos in
  f position

type ident = Ident of string [@@deriving compare]

type is_static = Is_static of bool

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
  | LocalVarDecl of is_static * vartype * ident * expr option * position                   
  
and case =
	| Case of expr * stmt list * position
	| Default of stmt list * position
	
type decl =
  | GlobalVarDecl of is_static * vartype * ident * expr option * position                      
  | FuncDecl of vartype * ident * (vartype * ident) list * stmt * position    
  | Typedef of vartype * vartype * position                                   
  | StructDecl of ident * decl list * position                               
  
type prog = Prog of decl list