open Core

module Position = 
  struct
    type t = {
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

module Expr = 
  struct
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

    type t =
      | IntLiteral of int * Position.t
      | FloatLiteral of float * Position.t
      | CharLiteral of char * Position.t
      | LongLiteral of int * Position.t
      | StringLiteral of string * Position.t
      | MemberAccess of t * Ident.t * Position.t
      | PointerMemberAccess of t * Ident.t * Position.t
      | ArrayAccess of t * t * Position.t
      | Var of Ident.t * Position.t
      | BinOp of bin_op * t * t * Position.t
      | Assign of Ident.t * t * Position.t
      | Call of Ident.t * t list * Position.t
      | PrefixUnOp of prefix_un_op * t * Position.t
      | PostfixUnOp of t * postfix_un_op * Position.t
    [@@deriving compare, sexp, equal, show]
  end


type is_static = Is_static of bool
[@@deriving compare, sexp, equal, show]

type var_decl = Var_decl of is_static * VarType.t * Ident.t * Expr.t option * Position.t 
[@@deriving compare, sexp, equal, show]

type typedef_decl = Typedef_decl of VarType.t * Ident.t * Position.t  
[@@deriving compare, sexp, equal, show]

  type struct_decl = Struct_decl of Ident.t * Ident.t * var_decl list option * Position.t
[@@deriving compare, sexp, equal, show]

type struct_init = Struct_init of Ident.t * Ident.t * Expr.t option * Position.t
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