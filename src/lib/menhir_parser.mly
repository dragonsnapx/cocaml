%{
  open Syntax_node

  (* Helper function to unwrap Lexing positions *)
  let make_position (startpos: Lexing.position) (endpos: Lexing.position)  =
    Position.create startpos.pos_cnum endpos.pos_cnum

  exception ParserError of string * Lexing.position

  let raise_parser_error (msg: string) (startpos: Lexing.position) =
    let line = startpos.pos_lnum in
    let col = startpos.pos_cnum - startpos.pos_bol + 1 in
    raise (ParserError (Printf.sprintf "Syntax error at line %d, column %d: %s" line col msg, startpos))

%}

(* Tokens with precedence and non-associativity *)
%left OR AND
%left EQUAL NOT_EQUAL
%left LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%left LEFT_SHIFT RIGHT_SHIFT
%left PLUS MINUS STAR SLASH PERCENT
%right ASSIGN PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL PERCENT_EQUAL
%nonassoc DOT ARROW
%nonassoc INCREMENT DECREMENT

(* Tokens List *)
%token BREAK CASE CHAR CONTINUE DEFAULT DOUBLE ELSE
%token FLOAT FOR IF INT LONG RETURN STRUCT SWITCH TYPEDEF STATIC VOID DO WHILE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <int> LONG_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA ASSIGN COLON
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL AMPERSAND_EQUAL BIT_OR_EQUAL BIT_XOR_EQUAL
%token LEFT_SHIFT RIGHT_SHIFT
%token PLUS_EQUAL MINUS_EQUAL STAR_EQUAL SLASH_EQUAL PERCENT_EQUAL
%token DOT ARROW LBRACKET RBRACKET
%token AND OR NOT
%token BIT_OR BIT_XOR BIT_NOT
%token AMPERSAND
%token INCREMENT DECREMENT
%token EOF

(* Main type from Syntax_node *)
%type <Syntax_node.prog> program

(* Helper types *)
%type <Syntax_node.Decl.t list> decl_list
%type <Syntax_node.Decl.t> decl
%type <Syntax_node.var_decl> var_decl
%type <Syntax_node.typedef_decl> typedef_decl
%type <Syntax_node.struct_decl> struct_decl
%type <Syntax_node.struct_init> struct_init
%type <Syntax_node.Stmt.t list> stmt_list
%type <Syntax_node.Stmt.t> stmt
%type <Syntax_node.Expr.t> expr
%type <Syntax_node.Expr.t list> expr_list
%type <Syntax_node.Expr.bin_op> bin_ops
%type <Syntax_node.Expr.prefix_un_op> prefix_un_ops
%type <Syntax_node.Expr.postfix_un_op> postfix_un_ops
%type <Syntax_node.VarType.t> type_spec
%type <Syntax_node.Expr.t list> arg_list
%type <Syntax_node.Stmt.case> case
%type <Syntax_node.Stmt.case list> case_list
%type <Syntax_node.Stmt.for_init> for_init
%type <Syntax_node.var_decl list> var_decl_list

(*
%type <(Syntax_node.VarType.t * Syntax_node.Ident.t) list> param_list
%type <(Syntax_node.VarType.t * Syntax_node.Ident.t)> param_list_non_empty
*)

(* Entry point *)
%start program

%%

(*
 * At the top-level, our program is nothing but a sequence of declarations (mostly function calls).
 * All statements, expressions, and other lower-level components should occur within these declarations.
 *)
program:
  | decl_list EOF { Prog $1 }


(****************************)
(* Part #0: Shared Types    *)
(****************************)

var_decl_list:
  | { [] }
  | var_decl_list var_decl { $1 @ [$2] }

var_decl:
  | STATIC type_spec IDENT ASSIGN expr SEMI {
      (* Example: static int x = 42; *)
      Var_decl (Is_static true, $2, Ident $3, Some $5, make_position $startpos $endpos)
    }
  | STATIC type_spec IDENT SEMI {
      (* Example: static int x; *)
      Var_decl (Is_static true, $2, Ident $3, None, make_position $startpos $endpos)
    }
  | STATIC type_spec IDENT LBRACKET INT_LITERAL RBRACKET ASSIGN expr SEMI {
      (* Example: static int arr[5] = {1, 2, 3, 4, 5}; *)
      Var_decl (Is_static true, VarType.Array ($2, Some $5), Ident $3, Some $8, make_position $startpos $endpos)
    }
  | STATIC type_spec IDENT LBRACKET INT_LITERAL RBRACKET SEMI {
      (* Example: static int arr[5]; *)
      Var_decl (Is_static true, VarType.Array ($2, Some $5), Ident $3, None, make_position $startpos $endpos)
    }
  | type_spec IDENT ASSIGN expr SEMI {
      (* Example: int x = 42; *)
      Var_decl (Is_static false, $1, Ident $2, Some $4, make_position $startpos $endpos)
    }
  | type_spec IDENT SEMI {
      (* Example: int x; *)
      Var_decl (Is_static false, $1, Ident $2, None, make_position $startpos $endpos)
    }
  | type_spec IDENT LBRACKET INT_LITERAL RBRACKET ASSIGN expr SEMI {
      (* Example: int arr[5] = {1, 2, 3, 4, 5}; *)
      Var_decl (Is_static false, VarType.Array ($1, Some $4), Ident $2, Some $7, make_position $startpos $endpos)
    }
  | type_spec IDENT LBRACKET INT_LITERAL RBRACKET SEMI {
      (* Example: int arr[5]; *)
      Var_decl (Is_static false, VarType.Array ($1, Some $4), Ident $2, None, make_position $startpos $endpos)
    }

typedef_decl:
  | TYPEDEF type_spec IDENT SEMI {
      (* Example: typedef int MyInt; *)
      Typedef_decl ($2, Ident $3, make_position $startpos $endpos)
    }

struct_decl:
  | STRUCT IDENT LBRACE var_decl_list RBRACE SEMI {
      (* Example: struct Point { int x; int y; }; *)
      Struct_decl (Ident $2, Ident $2, Some $4, make_position $startpos $endpos)
    }
  | STRUCT IDENT SEMI {
      (* Example: struct Point; *)
      Struct_decl (Ident $2, Ident $2, None, make_position $startpos $endpos)
    }

struct_init:
  | STRUCT IDENT IDENT ASSIGN expr SEMI {
      (* Example: struct Point p = {1, 2}; *)
      Struct_init (Ident $2, Ident $3, Some $5, make_position $startpos $endpos)
    }
  | STRUCT IDENT IDENT SEMI {
      (* Example: struct Point p; *)
      Struct_init (Ident $2, Ident $3, None, make_position $startpos $endpos)
    }
  

(****************************)
(* Part #1: Declarations    *)
(****************************)

decl_list:
  | { [] }               
  | decl_list decl { $1 @ [$2] } 

decl:
  (* Shared Cases *)
  | var_decl {
      Decl.VarDecl $1
    }
  | typedef_decl {
      Decl.TypedefDecl $1
    }
  | struct_decl {
      Decl.StructDecl $1
    }
  | struct_init {
      Decl.StructInit $1
    }

  (* Function Declarations *)
  | type_spec IDENT LPAREN param_list RPAREN stmt_block {
      (* Example: int add(int a, int b) { ... } *)
      Decl.FuncDecl ($1, Ident $2, $4, $6, make_position $startpos $endpos)
    }

  (* Error Handling *)
  | error SEMI {
      raise_parser_error "Invalid declaration" $startpos
    }

(* Function Parameters *)
param_list:
  | { [] }
  | param_list_non_empty { $1 }

param_list_non_empty:
  | type_spec IDENT { [($1, Ident $2)] }
  | param_list_non_empty COMMA type_spec IDENT { $1 @ [($3, Ident.create $4)] }


(****************************)
(* Part #2: Statements      *)
(****************************)

(* 
 * Note: Statements are more general than declarations. Function declarations contain stmt_blocks, which wrap 
 * stmt_lists. 
 *)

stmt_block:
  | LBRACE stmt_list RBRACE { Stmt.Block ($2, make_position $startpos $endpos) }

stmt_list:
  | { [] }
  | stmt_list stmt { $1 @ [$2] }

stmt:
  (* Shared Cases *)
  | struct_decl {
      Stmt.StructDecl $1
    }
  | struct_init {
      Stmt.StructInit $1
    }
  | typedef_decl {
      Stmt.TypedefDecl $1
    }
  | var_decl {
      Stmt.VarDecl $1
    }

  (* Return Statements *)
  | RETURN expr SEMI {
      Stmt.Return ($2, make_position $startpos $endpos)
    }

  (* If Statements *)
  | IF LPAREN expr RPAREN stmt ELSE stmt {
      Stmt.If ($3, $5, Some $7, make_position $startpos $endpos)
    }
  | IF LPAREN expr RPAREN stmt {
      Stmt.If ($3, $5, None, make_position $startpos $endpos)
    }

  (* Do-while Loops *)
  | DO stmt WHILE LPAREN expr RPAREN SEMI {
      Stmt.DoWhile ($2, $5, make_position $startpos $endpos)
    }

  (* While Loops *)
  | WHILE LPAREN expr RPAREN stmt {
      Stmt.While ($3, $5, make_position $startpos $endpos)
    }

  (* For Loops *)
  | FOR LPAREN for_init SEMI expr SEMI expr RPAREN stmt {
      Stmt.For ($3, $5, $7, $9, make_position $startpos $endpos)
    }

  (* Break Statement *)
  | BREAK SEMI {
      Stmt.Break (make_position $startpos $endpos)
    }

  (* Continue Statement *)
  | CONTINUE SEMI {
      Stmt.Continue (make_position $startpos $endpos)
    }

  (* Switch Statements *)
  | SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE {
      Stmt.Switch ($3, $6, make_position $startpos $endpos)
    }

  (* Expression Statements *)
  | expr SEMI {
      Stmt.ExprStmt ($1, make_position $startpos $endpos)
    }

  (* Nested Blocks *)
  | stmt_block {
      $1
    }

  (* Error Handling *)
  | error SEMI {
      raise_parser_error "Invalid statement" $startpos
    }


(* For-loop initializer can be either a variable declaration or assignment *)
for_init:
  | expr {
      Stmt.ForExpr $1
    }
  | var_decl {
      Stmt.ForVarDecl $1
    }

(* Cases for switch statements *)
case_list:
  | { [] }
  | case_list case { $1 @ [$2] }

case:
  | CASE expr COLON stmt_list {
      Stmt.Case ($2, $4, make_position $startpos $endpos)
    }
  | DEFAULT COLON stmt_list {
      Stmt.Default ($3, make_position $startpos $endpos)
    }


(*****************************)
(* Part #3: Expressions      *)
(*****************************)

(* 
 * Note: Expressions are the basic building blocks of statements, by enforcing local relationships between tokens.
 *)
expr:
  | INT_LITERAL {
      Expr.IntLiteral ($1, make_position $startpos $endpos)
    }
  | LONG_LITERAL {
      Expr.LongLiteral ($1, make_position $startpos $endpos)
    }
  | FLOAT_LITERAL {
      Expr.FloatLiteral ($1, make_position $startpos $endpos)
    }
  | CHAR_LITERAL {
      Expr.CharLiteral ($1, make_position $startpos $endpos)
    }
  | STRING_LITERAL {
      Expr.StringLiteral ($1, make_position $startpos $endpos)
    }
  | expr DOT IDENT {
      Expr.MemberAccess ($1, Ident $3, make_position $startpos $endpos)
    }
  | expr ARROW IDENT {
      Expr.PointerMemberAccess ($1, Ident $3, make_position $startpos $endpos)
    }
  | expr LBRACKET expr RBRACKET {
      Expr.ArrayAccess ($1, $3, make_position $startpos $endpos)
    }
  | expr ASSIGN expr {
      Expr.Assign ($1, $3, make_position $startpos $endpos)
    }
  | IDENT LPAREN arg_list RPAREN {
      Expr.Call (Ident $1, $3, make_position $startpos $endpos)
    }
  | IDENT {
      Expr.Var (Ident $1, make_position $startpos $endpos)
    }
  | expr bin_ops expr {
      Expr.BinOp ($2, $1, $3, make_position $startpos $endpos)
    }
  | prefix_un_ops expr {
      Expr.PrefixUnOp ($1, $2, make_position $startpos $endpos)
    }
  | expr postfix_un_ops {
      Expr.PostfixUnOp ($1, $2, make_position $startpos $endpos)
    }
  | LPAREN expr RPAREN {
      $2
    }
  | error {
      raise_parser_error "Invalid expression" $startpos
    }


(* Arguments for function calls *)
arg_list:
  | { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }


(***********************************)
(* Part #4: Types and Operations   *)
(***********************************)

(*
 * Note: Atomic units of our grammar. Every token in our lexer should be matched to at least one of these 
 *)
type_spec:
  | INT                                   { VarType.Int }
  | FLOAT                                 { VarType.Float }
  | CHAR                                  { VarType.Char }
  | DOUBLE                                { VarType.Double }
  | LONG                                  { VarType.Long }
  | VOID                                  { VarType.Void }
  | STRUCT IDENT                          { VarType.Struct (Ident $2) }
  | IDENT                                 { VarType.Typedef (Ident $1) }

bin_ops:
  | PLUS                                  { Expr.Plus }
  | MINUS                                 { Expr.Minus }
  | STAR                                  { Expr.Times }
  | SLASH                                 { Expr.Divide }
  | PERCENT                               { Expr.Modulo }
  | LEFT_SHIFT                            { Expr.LeftShift }
  | RIGHT_SHIFT                           { Expr.RightShift }
  | EQUAL                                 { Expr.Equal }
  | NOT_EQUAL                             { Expr.NotEqual }
  | LESS_THAN                             { Expr.Less }
  | LESS_EQUAL                            { Expr.LessEqual }
  | GREATER_THAN                          { Expr.Greater }
  | GREATER_EQUAL                         { Expr.GreaterEqual }
  | AND                                   { Expr.LogicalAnd }
  | OR                                    { Expr.LogicalOr }
  | BIT_OR                                { Expr.BitwiseOr }
  | BIT_XOR                               { Expr.BitwiseXor }
  | AMPERSAND                             { Expr.BitwiseAnd }
  | PLUS_EQUAL                            { Expr.PlusAssign }
  | MINUS_EQUAL                           { Expr.MinusAssign }
  | STAR_EQUAL                            { Expr.TimesAssign }
  | SLASH_EQUAL                           { Expr.DivideAssign }
  | PERCENT_EQUAL                         { Expr.ModuloAssign }
  | AMPERSAND_EQUAL                       { Expr.BitwiseAndAssign }
  | BIT_OR_EQUAL                          { Expr.BitwiseOrAssign }
  | BIT_XOR_EQUAL                         { Expr.BitwiseXorAssign }

prefix_un_ops:
  | PLUS                                  { Expr.Positive }
  | MINUS                                 { Expr.Negative }
  | NOT                                   { Expr.LogicalNot }
  | STAR                                  { Expr.Dereference }
  | AMPERSAND                             { Expr.Address }
  | BIT_NOT                               { Expr.BitwiseNot }
  | INCREMENT                             { Expr.PrefixIncrement }
  | DECREMENT                             { Expr.PrefixDecrement }

postfix_un_ops:
  | INCREMENT                             { Expr.PostfixIncrement }
  | DECREMENT                             { Expr.PostfixDecrement }
  
