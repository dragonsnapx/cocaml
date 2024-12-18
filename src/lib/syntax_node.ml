open Core

type position = {
  pos_start: int; 
  pos_end: int;
} [@@deriving compare, sexp, equal, show]

    let create (pos_start: int) (pos_end: int) : t = { pos_start; pos_end }
  end

module Ident = 
  struct
    type t = Ident of string [@@deriving compare, sexp, hash, equal, show]
    let create (name: string) : t = Ident name
  end

module VarType = 
  struct
    type t =
      | Int
      | Float
      | Char
      | Long
      | Double
      | Void
      | Struct of Ident.t
      | Typedef of Ident.t
      | Pointer of t
      | Array of t * int option
    [@@deriving compare, sexp, equal, show]
  end

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
  | BitwiseAnd    
  | BitwiseOr     
  | BitwiseXor 
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | ModuloAssign
  | BitwiseAndAssign   
  | BitwiseOrAssign    
  | BitwiseXorAssign
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
  | LongLiteral of int * position
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
  | StructVarDecl of ident * ident * position      
  | StructVarDeclInit of ident * ident * expr * position           
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

module Stmt = 
  (* Each type of statement for a program *)
  struct
    type t =
      | Return of Expr.t * Position.t
      | If of Expr.t * t * t option * Position.t
      | While of Expr.t * t * Position.t
      | For of for_init * Expr.t * Expr.t * t * Position.t
      | ExprStmt of Expr.t * Position.t
      | Block of t list * Position.t
      | Switch of Expr.t * case list * Position.t
      | Break of Position.t
      | Continue of Position.t
      | DoWhile of t * Expr.t * Position.t
      | VarDecl of var_decl
      | TypedefDecl of typedef_decl
      | StructDecl of struct_decl
      | StructInit of struct_init
    [@@deriving compare, sexp, equal, show]
  
    and case =
      | Case of Expr.t * t list * Position.t
      | Default of t list * Position.t
    [@@deriving compare, sexp, equal, show]

    and for_init =
      | ForExpr of Expr.t                                                                                           
      | ForVarDecl of var_decl                                                                         
    [@@deriving compare, sexp, equal, show]
  end

module Decl = 
  struct
    type t =    
      | VarDecl of var_decl 
      | FuncDecl of VarType.t * Ident.t * (VarType.t * Ident.t) list * Stmt.t * Position.t
      | TypedefDecl of typedef_decl
      | StructDecl of struct_decl
      | StructInit of struct_init
    [@@deriving compare, sexp, equal, show]
  end

type prog = Prog of Decl.t list [@@deriving compare, sexp, equal, show]
