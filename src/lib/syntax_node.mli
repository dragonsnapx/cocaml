module Position :
  sig
    type t = {
      pos_start: int;
      pos_end: int;
    } [@@deriving compare, sexp, equal, show]

    val create: int -> int -> t
  end

module Ident :
  (* Encompass names of variables, functions, etc. *)
  sig
    type t = Ident of string [@@deriving compare, sexp, hash, equal, show]

    val create: string -> t
  end

module VarType :
  sig
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

module Expr :
  sig
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
      | Assign of t * t * Position.t
      | Call of Ident.t * t list * Position.t
      | PrefixUnOp of prefix_un_op * t * Position.t
      | PostfixUnOp of t * postfix_un_op * Position.t
    [@@deriving compare, sexp, equal, show]
  end


(* 
 * Shared Types between statements and top-level declarations
 *)

(* Flag whether or not a variable/function is static *)
type is_static = Is_static of bool
[@@deriving compare, sexp, equal, show]

(* Variable Declarations, i.e. int x = 0; *)
type var_decl = Var_decl of is_static * VarType.t * Ident.t * Expr.t option * Position.t 
[@@deriving compare, sexp, equal, show]

(* Typedef Declarations, i.e. typedef MyInt int; *)
type typedef_decl = Typedef_decl of VarType.t * Ident.t * Position.t  
[@@deriving compare, sexp, equal, show]

(* Struct Declarations, i.e. struct Pair; or struct Pair {int x; int y};*)
type struct_decl = Struct_decl of Ident.t * Ident.t * var_decl list option * Position.t
[@@deriving compare, sexp, equal, show]

(* Struct Initializations, i.e. struct Pair p; or struct Pair p = {int x; int y};*)
type struct_init = Struct_init of Ident.t * Ident.t * Expr.t option * Position.t
[@@deriving compare, sexp, equal, show]


module Stmt :
  (* Each type of statement for a program *)
  sig
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
      | ForExpr of Expr.t                                                                (* Example: for (x = 0; ...) *)                              
      | ForVarDecl of var_decl                                                           (* Example: for (int x = 0; ...) *)                   
    [@@deriving compare, sexp, equal, show]
  end

module Decl :
  (* Top-level components of a program: must be a variable, function, structure or typedef declaration *)
  sig
    type t =    
      | VarDecl of var_decl 
      | FuncDecl of VarType.t * Ident.t * (VarType.t * Ident.t) list * Stmt.t * Position.t
      | TypedefDecl of typedef_decl
      | StructDecl of struct_decl
      | StructInit of struct_init
    [@@deriving compare, sexp, equal, show]
  end

type prog = Prog of Decl.t list [@@deriving compare, sexp, equal, show]
val is_prog_empty : prog -> bool