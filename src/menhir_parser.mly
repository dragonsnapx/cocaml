%{
  open Syntax_Node
%}

(* Tokens *)
%token AUTO BREAK CASE CHAR CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN
%token FLOAT FOR IF INT LONG RETURN STATIC STRUCT SWITCH TYPEDEF UNSIGNED VOID WHILE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> IDENT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA ASSIGN COLON
%token PLUS MINUS STAR SLASH
%token EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%token AND OR NOT
%token AMPERSAND
%token EOF

(* Entry point *)
%start program
%type <Syntax_Node.t list> program

%%

(* Our program is nothing but a sequence of declarations and statements *)
program:
  | decl_list EOF { $1 }


(****************************)
(* Part #1: Type Specifiers *)
(****************************)

type_spec:
  | INT { "int" }
  | FLOAT { "float" }
  | CHAR { "char" }
  | DOUBLE { "double" }
  | LONG { "long" }
  | VOID { "void" }


(*************************)
(* Part #2: Declarations *)
(*************************)

decl:
  | type_spec IDENT SEMI {
      Syntax_Node.Decl (Syntax_Node.VarDecl ($2, None, Syntax_Node.create_position 0 0))  (* Position TBD *)
    }
  | type_spec IDENT ASSIGN expr SEMI {
      Syntax_Node.Decl (Syntax_Node.VarDecl ($2, Some $4, Syntax_Node.create_position 0 0))
    }
  | type_spec IDENT LPAREN param_list RPAREN stmt_block {
      Syntax_Node.Decl (Syntax_Node.FuncDecl ($2, $4, $6, Syntax_Node.create_position 0 0))
    }
  | TYPEDEF type_spec IDENT SEMI {
      Syntax_Node.Decl (Syntax_Node.Typedef ($2, $3, make_position ()))
    }
  | STRUCT IDENT LBRACE decl_list RBRACE SEMI {
      Syntax_Node.Decl (Syntax_Node.StructDecl ($2, $4, make_position ()))
    }
  | error SEMI {
      failwith "Syntax error in declaration."
    }

decl_list:
  | /* empty */ { [] }
  | decl_list decl { $1 @ [$2] }


(*************************)
(* Part #3: Statements   *)
(*************************)

stmt:
  | RETURN expr SEMI {
      Syntax_Node.Stmt (Syntax_Node.Return ($2, Syntax_Node.create_position 0 0))
    }
  | IF LPAREN expr RPAREN stmt ELSE stmt {
      Syntax_Node.Stmt (Syntax_Node.If ($3, $5, Some $7, Syntax_Node.create_position 0 0))
    }
  | IF LPAREN expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.If ($3, $5, None, Syntax_Node.create_position 0 0))
    }
  | WHILE LPAREN expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.While ($3, $5, Syntax_Node.create_position 0 0))
    }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.For ($3, $5, $7, $9, Syntax_Node.create_position 0 0))
    }
  | SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE {
      Syntax_Node.Stmt (
        Syntax_Node.Switch ($3, $6, Syntax_Node.create_position 0 0)
      )
    }
  | expr SEMI {
      Syntax_Node.Stmt (Syntax_Node.ExprStmt ($1, Syntax_Node.create_position 0 0))
    }
  | stmt_block {
      Syntax_Node.Stmt (Syntax_Node.Block ($1, Syntax_Node.create_position 0 0))
    }
  | error SEMI {
      failwith "Syntax error in statement."
    }

stmt_block:
  | LBRACE stmt_list RBRACE { $2 }

stmt_list:
  | /* empty */ { [] }
  | stmt_list stmt { $1 @ [$2] }


(**************************)
(* Part #4: Utility Lists *)
(**************************)

(* Expressions *)
expr:
  | INT_LITERAL {
      Syntax_Node.Expr (Syntax_Node.IntLiteral ($1, Syntax_Node.create_position 0 0))
    }
  | FLOAT_LITERAL {
      Syntax_Node.Expr (Syntax_Node.FloatLiteral ($1, Syntax_Node.create_position 0 0))
    }
  | CHAR_LITERAL {
      Syntax_Node.Expr (Syntax_Node.CharLiteral ($1, Syntax_Node.create_position 0 0))
    }
  | IDENT {
      Syntax_Node.Expr (Syntax_Node.Var ($1, Syntax_Node.create_position 0 0))
    }
  | IDENT LPAREN arg_list RPAREN {
      Syntax_Node.Expr (Syntax_Node.Call ($1, $3, Syntax_Node.create_position 0 0))
    }
  | expr PLUS expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("+", $1, $3, Syntax_Node.create_position 0 0))
    }
  | expr MINUS expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("-", $1, $3, Syntax_Node.create_position 0 0))
    }
  | expr STAR expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("*", $1, $3, make_position ()))
    }
  | expr SLASH expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("/", $1, $3, make_position ()))
    }
  | expr EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("==", $1, $3, make_position ()))
    }
  | expr NOT_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("!=", $1, $3, make_position ()))
    }
  | expr LESS_THAN expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("<", $1, $3, make_position ()))
    }
  | expr LESS_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("<=", $1, $3, make_position ()))
    }
  | expr GREATER_THAN expr {
      Syntax_Node.Expr (Syntax_Node.BinOp (">", $1, $3, make_position ()))
    }
  | expr GREATER_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp (">=", $1, $3, make_position ()))
    }
  | expr AND expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("&&", $1, $3, make_position ()))
    }
  | expr OR expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("||", $1, $3, make_position ()))
    }
  | NOT expr {
      Syntax_Node.Expr (Syntax_Node.UnOp ("!", $2, make_position ()))
    }
  | AMPERSAND expr {
      Syntax_Node.Expr (Syntax_Node.UnOp ("&", $2, make_position ()))
    }
  | LPAREN expr RPAREN { $2 }
  | error { failwith "Syntax error in expression." }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }


(* Function Parameters *)
param_list_non_empty:
  | IDENT { [$1] }
  | param_list_non_empty COMMA IDENT { $1 @ [$3] }

param_list:
  | /* empty */ { [] }
  | param_list_non_empty { $1 }


(* Function Arguments *)
arg_list:
  | /* empty */ { [] }
  | expr_list { $1 }


(* Cases for switch statements *)
case:
  | CASE expr COLON stmt_list {
      Syntax_Node.Case ($2, $4, make_position ())
    }
  | DEFAULT COLON stmt_list {
      Syntax_Node.Default ($3, make_position ())
    }

case_list:
  | /* empty */ { [] }
  | case_list case { $1 @ [$2] }


(*****************************)
(* Part #5: Helper Functions *)
(*****************************)
let make_position () =
  let pos_start = Lexing.lexeme_start_p lexbuf in
  let pos_end = Lexing.lexeme_end_p lexbuf in
  Syntax_Node.create_position pos_start.pos_cnum pos_end.pos_cnum
