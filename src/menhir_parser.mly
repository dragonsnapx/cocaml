%{
  (* Helper function to unwrap Lexing positions *)
  let make_position (startpos: Lexing.position) (endpos: Lexing.position)  =
    Syntax_node.create_position startpos.pos_cnum endpos.pos_cnum
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
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA ASSIGN COLON
%token PLUS MINUS STAR SLASH PERCENT
%token EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
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

(* Other types -- we currently have some types without corresponding types in Syntax_node *)
%type <Syntax_node.decl list> decl_list
%type <Syntax_node.decl> decl
%type <Syntax_node.expr> expr
%type <Syntax_node.expr list> expr_list
%type <Syntax_node.bin_op> bin_ops
%type <Syntax_node.prefix_un_op> prefix_un_ops
%type <Syntax_node.postfix_un_op> postfix_un_ops
%type <Syntax_node.stmt> stmt
%type <Syntax_node.stmt list> stmt_list
%type <Syntax_node.case> case
%type <Syntax_node.case list> case_list
%type <Syntax_node.vartype> type_spec

(* Entry point *)
%start program

%%

(*
 * At the top-level, our program is nothing but a sequence of declarations (mostly function calls).
 * All statements, expressions, and other lower-level components should occur within these declarations.
 *)
program:
  | decl_list EOF { Syntax_node.Prog $1 }


(****************************)
(* Part #1: Declarations    *)
(****************************)

decl_list:
  | { [] }               
  | decl_list decl { $1 @ [$2] } 

decl:
  | STATIC type_spec IDENT SEMI {
      Syntax_node.GlobalVarDecl (Syntax_node.Is_static true, $2, Syntax_node.Ident $3, None, make_position $startpos $endpos)
  }
  | STATIC type_spec IDENT ASSIGN expr SEMI {
      Syntax_node.GlobalVarDecl (Syntax_node.Is_static true, $2, Syntax_node.Ident $3, Some $5, make_position $startpos $endpos)
    }
  | type_spec IDENT SEMI {
      Syntax_node.GlobalVarDecl (Syntax_node.Is_static false, $1, Syntax_node.Ident $2, None, make_position $startpos $endpos)
    }
  | type_spec IDENT ASSIGN expr SEMI {
      Syntax_node.GlobalVarDecl (Syntax_node.Is_static false, $1, Syntax_node.Ident $2, Some $4, make_position $startpos $endpos)
    }
  | type_spec IDENT LPAREN param_list RPAREN stmt_block {
      Syntax_node.FuncDecl ($1, Syntax_node.Ident $2, $4, $6, make_position $startpos $endpos)
    }
  | TYPEDEF type_spec IDENT SEMI {
      Syntax_node.TypedefDecl ($2, Syntax_node.Ident $3, make_position $startpos $endpos)
    }
  | STRUCT IDENT LBRACE decl_list RBRACE SEMI {
      Syntax_node.StructDecl (Syntax_node.Ident $2, $4, make_position $startpos $endpos)
    }
  | error SEMI {
      Printf.eprintf "Syntax error in declaration.\n";
      Syntax_node.GlobalVarDecl (Syntax_node.Void, Syntax_node.Ident "error", None, make_position $startpos $endpos)
    }

(* Function parameters *)
param_list:
  | { [] }
  | param_list_non_empty { $1 }

param_list_non_empty:
  | type_spec IDENT { [($1, Syntax_node.Ident $2)] }
  | param_list_non_empty COMMA type_spec IDENT { $1 @ [($3, Syntax_node.Ident $4)] }


(****************************)
(* Part #2: Statements      *)
(****************************)

(* 
 * Note: Statements are more general than declarations. Function declarations contain stmt_blocks, which wrap 
 * stmt_lists. 
 *)

stmt_block:
  | LBRACE stmt_list RBRACE { Syntax_node.Block ($2, make_position $startpos $endpos) }

stmt_list:
  | { [] }
  | stmt_list stmt { $1 @ [$2] }

stmt:
  | RETURN expr SEMI {
      Syntax_node.Return ($2, make_position $startpos $endpos)
    }
  | IF LPAREN expr RPAREN stmt ELSE stmt {
      Syntax_node.If ($3, $5, Some $7, make_position $startpos $endpos)
    }
  | IF LPAREN expr RPAREN stmt {
      Syntax_node.If ($3, $5, None, make_position $startpos $endpos)
    }
  | WHILE LPAREN expr RPAREN stmt {
      Syntax_node.While ($3, $5, make_position $startpos $endpos)
    }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt {
      Syntax_node.For ($3, $5, $7, $9, make_position $startpos $endpos)
    }
  | expr SEMI {
      Syntax_node.ExprStmt ($1, make_position $startpos $endpos)
    }
  | stmt_block {
      $1 (* Used for nested statements -- i.e. if, while, etc. *)
    }
  | SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE {
      Syntax_node.Switch ($3, $6, make_position $startpos $endpos)
    }
  | DO stmt WHILE LPAREN expr RPAREN SEMI {
      Syntax_node.DoWhile ($2, $5, make_position $startpos $endpos)
    }
  | BREAK SEMI {
      Syntax_node.Break (make_position $startpos $endpos)
    }
  | CONTINUE SEMI {
      Syntax_node.Continue (make_position $startpos $endpos)
    }
  | type_spec IDENT SEMI {
      Syntax_node.LocalVarDecl (Syntax_node.Is_static false, $1, Syntax_node.Ident $2, None, make_position $startpos $endpos)
    }
  | type_spec IDENT ASSIGN expr SEMI {
      Syntax_node.LocalVarDecl (Syntax_node.Is_static false, $1, Syntax_node.Ident $2, Some $4, make_position $startpos $endpos)
    }
  | STATIC type_spec IDENT SEMI {
      Syntax_node.LocalVarDecl (Syntax_node.Is_static true, $2, Syntax_node.Ident $3, None, make_position $startpos $endpos)
    }
  | STATIC type_spec IDENT ASSIGN expr SEMI {
      Syntax_node.LocalVarDecl (Syntax_node.Is_static true, $2, Syntax_node.Ident $3, Some $5, make_position $startpos $endpos)
    }

(* Cases for switch statements *)
case_list:
  | { [] }
  | case_list case { $1 @ [$2] }

case:
  | CASE expr COLON stmt_list {
      Syntax_node.Case ($2, $4, make_position $startpos $endpos)
    }
  | DEFAULT COLON stmt_list {
      Syntax_node.Default ($3, make_position $startpos $endpos)
    }

(*****************************)
(* Part #3: Expressions      *)
(*****************************)

(* 
 * Note: Expressions are the basic building blocks of statements, by enforcing local relationships between tokens.
 *)
expr:
  | INT_LITERAL {
      Syntax_node.IntLiteral ($1, make_position $startpos $endpos)
    }
  | FLOAT_LITERAL {
      Syntax_node.FloatLiteral ($1, make_position $startpos $endpos)
    }
  | CHAR_LITERAL {
      Syntax_node.CharLiteral ($1, make_position $startpos $endpos)
    }
  | STRING_LITERAL {
      Syntax_node.StringLiteral ($1, make_position $startpos $endpos)
    }
  | expr DOT IDENT {
      Syntax_node.MemberAccess ($1, Syntax_node.Ident $3, make_position $startpos $endpos)
    }
  | expr ARROW IDENT {
      Syntax_node.PointerMemberAccess ($1, Syntax_node.Ident $3, make_position $startpos $endpos)
    }
  | expr LBRACKET expr RBRACKET {
      Syntax_node.ArrayAccess ($1, $3, make_position $startpos $endpos)
    }
  | IDENT {
      Syntax_node.Var (Syntax_node.Ident $1, make_position $startpos $endpos)
    }
  | IDENT ASSIGN expr {
      Syntax_node.Assign (Syntax_node.Ident $1, $3, make_position $startpos $endpos)
    }
  | IDENT LPAREN arg_list RPAREN {
      Syntax_node.Call (Syntax_node.Ident $1, $3, make_position $startpos $endpos)
    }
  | expr bin_ops expr {
      Syntax_node.BinOp ($2, $1, $3, make_position $startpos $endpos)
    }
  | prefix_un_ops expr {
      Syntax_node.PrefixUnOp ($1, $2, make_position $startpos $endpos)
    }
  | expr postfix_un_ops {
      Syntax_node.PostfixUnOp ($1, $2, make_position $startpos $endpos)
    }
  | LPAREN expr RPAREN {
      $2
    }
  | error {
      (* Note: The below is out of scope -- will have to figure out why *)
      (* Printf.eprintf "Syntax error in expression at line %d.\n" $startpos.pos_lnum; *)
      Printf.eprintf "Syntax error in expression";
      Syntax_node.Var (Syntax_node.Ident "error", make_position $startpos $endpos)
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
  | INT { Syntax_node.Int }
  | FLOAT { Syntax_node.Float }
  | CHAR { Syntax_node.Char }
  | DOUBLE { Syntax_node.Double }
  | LONG { Syntax_node.Long }
  | VOID { Syntax_node.Void }
  | STRUCT IDENT { Syntax_node.Struct (Syntax_node.Ident $2) }
  | IDENT { Syntax_node.Typedef (Syntax_node.Ident $1) }

bin_ops:
  | PLUS { Syntax_node.Plus }
  | MINUS { Syntax_node.Minus }
  | STAR { Syntax_node.Times }
  | SLASH { Syntax_node.Divide }
  | PERCENT { Syntax_node.Modulo }
  | LEFT_SHIFT { Syntax_node.LeftShift }
  | RIGHT_SHIFT { Syntax_node.RightShift }
  | EQUAL { Syntax_node.Equal }
  | NOT_EQUAL { Syntax_node.NotEqual }
  | LESS_THAN { Syntax_node.Less }
  | LESS_EQUAL { Syntax_node.LessEqual }
  | GREATER_THAN { Syntax_node.Greater }
  | GREATER_EQUAL { Syntax_node.GreaterEqual }
  | AND { Syntax_node.LogicalAnd }
  | OR { Syntax_node.LogicalOr }
  | PLUS_EQUAL { Syntax_node.PlusAssign }
  | MINUS_EQUAL { Syntax_node.MinusAssign }
  | STAR_EQUAL { Syntax_node.TimesAssign }
  | SLASH_EQUAL { Syntax_node.DivideAssign }
  | PERCENT_EQUAL { Syntax_node.ModuloAssign }
  | AMPERSAND { Syntax_node.BitwiseAnd }
  | BIT_OR { Syntax_node.BitwiseOr }
  | BIT_XOR { Syntax_node.BitwiseXor }

prefix_un_ops:
  | PLUS { Syntax_node.Positive}
  | MINUS { Syntax_node.Negative }
  | NOT { Syntax_node.LogicalNot }
  | STAR { Syntax_node.Dereference }
  | AMPERSAND { Syntax_node.Address }
  | BIT_NOT { Syntax_node.BitwiseNot }
  | INCREMENT { Syntax_node.PrefixIncrement }
  | DECREMENT { Syntax_node.PrefixDecrement }

postfix_un_ops:
  | INCREMENT { Syntax_node.PostfixIncrement }
  | DECREMENT { Syntax_node.PostfixDecrement }

