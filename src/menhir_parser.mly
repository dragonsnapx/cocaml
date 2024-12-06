%{
  open Syntax_node

  (* Helper function to compute positions from lexbuf *)
  let make_position lexbuf =
    let pos_start = Lexing.lexeme_start_p lexbuf in
    let pos_end = Lexing.lexeme_end_p lexbuf in
    Syntax_node.create_position pos_start.pos_cnum pos_end.pos_cnum
%}

(* Tokens with precedence and associativity *)
%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%left PLUS MINUS
%left STAR SLASH
%right NOT
%right ASSIGN

(* Tokens List *)
%token BREAK CASE CHAR CONTINUE DEFAULT DOUBLE ELSE
%token FLOAT FOR IF INT LONG RETURN STRUCT SWITCH TYPEDEF VOID DO WHILE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> IDENT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA ASSIGN COLON
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%token AND OR NOT
%token BIT_OR BIT_XOR BIT_NOT
%token AMPERSAND
%token EOF

(* Entry point *)
%type <Syntax_node.decl list> program
%start program

%%

(*
 * Our program is nothing but a sequence of declarations and statements. All statements
 * should occur within declarations (i.e. functions), meaning they'll be contained in decl_list.
 *)
program:
  | decl_list EOF { $1 }

decl_list:
  | { [] }               
  | decl_list decl { $1 @ [$2] } 

decl:
  | type_spec IDENT SEMI {
      Syntax_node.VarDecl ($1, Syntax_node.Ident $2, None, make_position lexbuf)
    }
  | type_spec IDENT ASSIGN expr SEMI {
      Syntax_node.VarDecl ($1, Syntax_node.Ident $2, Some $4, make_position lexbuf)
    }
  | type_spec IDENT LPAREN param_list RPAREN stmt_block {
      Syntax_node.FuncDecl ($1, Syntax_node.Ident $2, $4, $6, make_position lexbuf)
    }
  | TYPEDEF type_spec IDENT SEMI {
      Syntax_node.Typedef ($2, Syntax_node.Custom (Syntax_node.Ident $3), make_position lexbuf)
    }
  | STRUCT IDENT LBRACE decl_list RBRACE SEMI {
      Syntax_node.StructDecl (Syntax_node.Ident $2, $4, make_position lexbuf)
    }
  | error SEMI {
      Printf.eprintf "Syntax error in declaration.\n";
      Syntax_node.VarDecl (Syntax_node.Void, Syntax_node.Ident "error", None, make_position lexbuf)
    }

param_list:
  | { [] }
  | param_list_non_empty { $1 }

param_list_non_empty:
  | type_spec IDENT { [($1, Syntax_node.Ident $2)] }
  | param_list_non_empty COMMA type_spec IDENT { $1 @ [($3, Syntax_node.Ident $4)] }

stmt_block:
  | LBRACE stmt_list RBRACE { Syntax_node.Block ($2, Syntax_node.create_position 0 0) }
  (* | LBRACE stmt_list RBRACE { Syntax_node.Block ($2, make_position lexbuf) } *)

stmt_list:
  | { [] }
  | stmt_list stmt { $1 @ [$2] }

stmt:
  | RETURN expr SEMI {
      Syntax_node.Return ($2, make_position lexbuf)
    }
  | IF LPAREN expr RPAREN stmt ELSE stmt {
      Syntax_node.If ($3, $5, Some $7, make_position lexbuf)
    }
  | IF LPAREN expr RPAREN stmt {
      Syntax_node.If ($3, $5, None, make_position lexbuf)
    }
  | WHILE LPAREN expr RPAREN stmt {
      Syntax_node.While ($3, $5, make_position lexbuf)
    }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt {
      Syntax_node.For ($3, $5, $7, $9, make_position lexbuf)
    }
  | expr SEMI {
      Syntax_node.ExprStmt ($1, make_position lexbuf)
    }
  | stmt_block {
      $1 (* Syntax_node.Block ($1, make_position lexbuf) *)
    }
  | SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE {
      Syntax_node.Switch ($3, $6, make_position lexbuf)
    }
  | DO stmt WHILE LPAREN expr RPAREN SEMI {
      Syntax_node.DoWhile ($2, $5, make_position lexbuf)
    }
  | BREAK SEMI {
      Syntax_node.Break (make_position lexbuf)
    }
  | CONTINUE SEMI {
      Syntax_node.Continue (make_position lexbuf)
    }

case_list:
  | { [] }
  | case_list case { $1 @ [$2] }

case:
  | CASE expr COLON stmt_list {
      Syntax_node.Case ($2, $4, make_position lexbuf)
    }
  | DEFAULT COLON stmt_list {
      Syntax_node.Default ($3, make_position lexbuf)
    }

expr:
  | INT_LITERAL {
      Syntax_node.IntLiteral ($1, make_position lexbuf)
    }
  | FLOAT_LITERAL {
      Syntax_node.FloatLiteral ($1, make_position lexbuf)
    }
  | CHAR_LITERAL {
      Syntax_node.CharLiteral ($1, make_position lexbuf)
    }
  | IDENT {
      Syntax_node.Var (Syntax_node.Ident $1, make_position lexbuf)
    }
  | IDENT ASSIGN expr {
      Syntax_node.Assign (Syntax_node.Ident $1, $3, make_position lexbuf)
    }
  | IDENT LPAREN arg_list RPAREN {
      Syntax_node.Call (Syntax_node.Ident $1, $3, make_position lexbuf)
    }
  | expr bin_ops expr {
      Syntax_node.BinOp ($2, $1, $3, make_position lexbuf)
    }
  | un_ops expr {
      Syntax_node.UnOp ($1, $2, make_position lexbuf)
    }
  | LPAREN expr RPAREN {
      $2
    }
  | error {
      Printf.eprintf "Syntax error in expression at line %d.\n" (Lexing.lexeme_start_p lexbuf).pos_lnum;
      Syntax_node.Var ("error", make_position lexbuf)
    }

arg_list:
  | { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }

bin_ops:
  | PLUS { Syntax_node.Plus }
  | MINUS { Syntax_node.Minus }
  | STAR { Syntax_node.Times }
  | SLASH { Syntax_node.Divide }
  | PERCENT { Syntax_node.Modulo }
  | EQUAL { Syntax_node.Equal }
  | NOT_EQUAL { Syntax_node.NotEqual }
  | LESS_THAN { Syntax_node.Less }
  | LESS_EQUAL { Syntax_node.LessEqual }
  | GREATER_THAN { Syntax_node.Greater }
  | GREATER_EQUAL { Syntax_node.GreaterEqual }
  | AND { Syntax_node.LogicalAnd }
  | OR { Syntax_node.LogicalOr }
  | AMPERSAND { Syntax_node.BitwiseAnd }
  | BIT_OR { Syntax_node.BitwiseOr }
  | BIT_XOR { Syntax_node.BitwiseXor }

un_ops:
  | PLUS { Syntax_node.Positive}
  | MINUS { Syntax_node.Negative }
  | NOT { Syntax_node.LogicalNot }
  | STAR { Syntax_node.Dereference }
  | AMPERSAND { Syntax_node.Address }
  | BIT_NOT { Syntax_node.BitwiseNot }

type_spec:
  | INT { Syntax_node.Int }
  | FLOAT { Syntax_node.Float }
  | CHAR { Syntax_node.Char }
  | DOUBLE { Syntax_node.Double }
  | LONG { Syntax_node.Long }
  | VOID { Syntax_node.Void }
  | IDENT { Syntax_node.Custom (Syntax_node.Ident $1) }

