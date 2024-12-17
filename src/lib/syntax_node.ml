open Core

type position = {
  pos_start: int; 
  pos_end: int;
} [@@deriving compare, sexp, equal, show]

let create_position start_pos end_pos = { pos_start = start_pos; pos_end = end_pos }

type ident = Ident of string [@@deriving compare, sexp, equal, show]

type is_static = Is_static of bool [@@deriving compare, sexp, equal, show]

type vartype =
  | Int
  | Float
  | Char
  | Long
  | Double
  | Void
  | Struct of ident
  | Typedef of ident
  | Pointer of vartype
[@@deriving compare, sexp, equal, show]

type bin_op =
  | Plus           
  | Minus          
  | Times         
  | Divide         
  | Modulo    
  | LeftShift
  | RightShift     
  | Equal          
  | NotEqual       
  | Less           
  | LessEqual      
  | Greater       
  | GreaterEqual   
  | LogicalAnd     
  | LogicalOr     
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | ModuloAssign
  | BitwiseAnd    
  | BitwiseOr     
  | BitwiseXor
[@@deriving compare, sexp, equal, show]

type prefix_un_op =
  | Positive
  | Negative         
  | LogicalNot   
  | BitwiseNot 
  | Address        
  | Dereference    
  | PrefixIncrement
  | PrefixDecrement
[@@deriving compare, sexp, equal, show]

type postfix_un_op = 
  | PostfixIncrement
  | PostfixDecrement
[@@deriving compare, sexp, equal, show]

type expr =
  | IntLiteral of int * position
  | FloatLiteral of float * position
  | CharLiteral of char * position
  | StringLiteral of string * position
  | MemberAccess of expr * ident * position
  | PointerMemberAccess of expr * ident * position
  | ArrayAccess of expr * expr * position
  | Var of ident * position
  | BinOp of bin_op * expr * expr * position
  | Assign of ident * expr * position
  | Call of ident * expr list * position
  | PrefixUnOp of prefix_un_op * expr * position
  | PostfixUnOp of expr * postfix_un_op * position
[@@deriving compare, sexp, equal, show]

type stmt =
  | Return of expr * position
  | If of expr * stmt * stmt option * position
  | While of expr * stmt * position
  | For of for_init * expr * expr * stmt * position
  | ExprStmt of expr * position
  | Block of stmt list * position
  | Switch of expr * case list * position
  | Break of position
  | Continue of position
  | DoWhile of stmt * expr * position
  | LocalVarDecl of is_static * vartype * ident * expr option * position                   
[@@deriving compare, sexp, equal, show]

and for_init =
  | ForExpr of expr                                                           
  | ForVarDecl of is_static * vartype * ident * expr option                   
[@@deriving compare, sexp, equal, show]

and case =
	| Case of expr * stmt list * position
	| Default of stmt list * position
[@@deriving compare, sexp, equal, show]
	
type decl =
  | GlobalVarDecl of is_static * vartype * ident * expr option * position     
  | FuncDecl of vartype * ident * (vartype * ident) list * stmt * position    
  | TypedefDecl of vartype * ident * position                                 
  | StructDecl of ident * decl list * position                               
[@@deriving compare, sexp, equal, show]

type prog = Prog of decl list [@@deriving compare, sexp, equal, show]