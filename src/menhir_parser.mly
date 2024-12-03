%{
  open Syntax_node

  (* Helper function to compute positions from lexbuf *)
  let make_position lexbuf =
    let pos_start = Lexing.lexeme_start_p lexbuf in
    let pos_end = Lexing.lexeme_end_p lexbuf in
    Syntax_node.create_position pos_start.pos_cnum pos_end.pos_cnum
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
%type <Syntax_node.t list> program

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
      Syntax_Node.Decl (Syntax_node.VarDecl ($2, None, make_position lexbuf)) 
    }
  | type_spec IDENT ASSIGN expr SEMI {
      Syntax_Node.Decl (Syntax_Node.VarDecl ($2, Some $4, make_position lexbuf))
    }
  | type_spec IDENT LPAREN param_list RPAREN stmt_block {
      Syntax_Node.Decl (Syntax_Node.FuncDecl ($2, $4, $6, make_position lexbuf))
    }
  | TYPEDEF type_spec IDENT SEMI {
      Syntax_Node.Decl (Syntax_Node.Typedef ($2, $3, make_position lexbuf))
    }
  | STRUCT IDENT LBRACE decl_list RBRACE SEMI {
      Syntax_Node.Decl (Syntax_Node.StructDecl ($2, $4, make_position lexbuf))
    }
  | error SEMI {
      Printf.eprintf "Syntax error in declaration.\n";
      Syntax_Node.Decl (Syntax_Node.VarDecl ("error", None, make_position lexbuf))
    }

decl_list:
  | /* empty */ { [] }
  | decl_list decl { $1 @ [$2] }


(*************************)
(* Part #3: Statements   *)
(*************************)

stmt:
  | RETURN expr SEMI {
      Syntax_Node.Stmt (Syntax_Node.Return ($2, make_position lexbuf))
    }
  | IF LPAREN expr RPAREN stmt ELSE stmt {
      Syntax_Node.Stmt (Syntax_Node.If ($3, $5, Some $7, make_position lexbuf))
    }
  | IF LPAREN expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.If ($3, $5, None, make_position lexbuf))
    }
  | WHILE LPAREN expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.While ($3, $5, make_position lexbuf))
    }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt {
      Syntax_Node.Stmt (Syntax_Node.For ($3, $5, $7, $9, make_position lexbuf))
    }
  | SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE {
      Syntax_Node.Stmt (Syntax_Node.Switch ($3, $6, make_position lexbuf))
    }
  | expr SEMI {
      Syntax_Node.Stmt (Syntax_Node.ExprStmt ($1, make_position lexbuf))
    }
  | stmt_block {
      Syntax_Node.Stmt (Syntax_Node.Block ($1, make_position lexbuf))
    }
  | error SEMI {
      Printf.eprintf "Syntax error in statement at line %d.\n" (Lexing.lexeme_start_p lexbuf).pos_lnum;
      Syntax_Node.Stmt (Syntax_Node.ExprStmt (
        Syntax_Node.Var ("error", make_position lexbuf), make_position lexbuf))
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
      Syntax_Node.Expr (Syntax_Node.IntLiteral ($1, make_position lexbuf))
    }
  | FLOAT_LITERAL {
      Syntax_Node.Expr (Syntax_Node.FloatLiteral ($1, make_position lexbuf))
    }
  | CHAR_LITERAL {
      Syntax_Node.Expr (Syntax_Node.CharLiteral ($1, make_position lexbuf))
    }
  | IDENT {
      Syntax_Node.Expr (Syntax_Node.Var ($1, make_position lexbuf))
    }
  | IDENT LPAREN arg_list RPAREN {
      Syntax_Node.Expr (Syntax_Node.Call ($1, $3, make_position lexbuf))
    }
  | expr PLUS expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("+", $1, $3, make_position lexbuf))
    }
  | expr MINUS expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("-", $1, $3, make_position lexbuf))
    }
  | expr STAR expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("*", $1, $3, make_position lexbuf))
    }
  | expr SLASH expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("/", $1, $3, make_position lexbuf))
    }
  | expr EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("==", $1, $3, make_position lexbuf))
    }
  | expr NOT_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("!=", $1, $3, make_position lexbuf))
    }
  | expr LESS_THAN expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("<", $1, $3, make_position lexbuf))
    }
  | expr LESS_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("<=", $1, $3, make_position lexbuf))
    }
  | expr GREATER_THAN expr {
      Syntax_Node.Expr (Syntax_Node.BinOp (">", $1, $3, make_position lexbuf))
    }
  | expr GREATER_EQUAL expr {
      Syntax_Node.Expr (Syntax_Node.BinOp (">=", $1, $3, make_position lexbuf))
    }
  | expr AND expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("&&", $1, $3, make_position lexbuf))
    }
  | expr OR expr {
      Syntax_Node.Expr (Syntax_Node.BinOp ("||", $1, $3, make_position lexbuf))
    }
  | NOT expr {
      Syntax_Node.Expr (Syntax_Node.UnOp ("!", $2, make_position lexbuf))
    }
  | AMPERSAND expr {
      Syntax_Node.Expr (Syntax_Node.UnOp ("&", $2, make_position lexbuf))
    }
  | LPAREN expr RPAREN { $2 }
  | error {
      Printf.eprintf "Syntax error in expression at line %d.\n" (Lexing.lexeme_start_p lexbuf).pos_lnum;
      Syntax_Node.Expr (Syntax_Node.Var ("error", make_position lexbuf))
    }

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
      Syntax_Node.Case ($2, $4, make_position lexbuf)
    }
  | DEFAULT COLON stmt_list {
      Syntax_Node.Default ($3, make_position lexbuf)
    }

case_list:
  | /* empty */ { [] }
  | case_list case { $1 @ [$2] }

