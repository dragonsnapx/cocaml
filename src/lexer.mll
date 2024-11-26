{
  open Parser
  open Lexing

  exception LexerError of string * Lexing.position

  let raise_error msg lexbuf =
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    raise (LexerError (Printf.sprintf "Line %d, column %d: %s" line col msg, pos))
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let comment_single = "//" [^'\n']* '\n'
let comment_multi = "/*" [^'*']* '*' ('/' | [^'/'] [^'*']* '*')* "*/"

rule token = parse
  | "int"          { INT }
  | "float"        { FLOAT }
  | "char"         { CHAR }
  | "long"         { LONG }
  | "double"       { DOUBLE }
  | "return"       { RETURN }
  | "if"           { IF }
  | "else"         { ELSE }
  | "for"          { FOR }
  | "while"        { WHILE }
  | "do"           { DO }
  | "switch"       { SWITCH }
  | "case"         { CASE }
  | "default"      { DEFAULT }
  | "struct"       { STRUCT }
  | digit+         { INT_LITERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | digit+ '.' digit+ { FLOAT_LITERAL (float_of_string (Lexing.lexeme lexbuf)) }
  | '\'' [^'\''] '\'' { CHAR_LITERAL (Lexing.lexeme lexbuf).[1] }
  | identifier     { IDENT (Lexing.lexeme lexbuf) }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "{"            { LBRACE }
  | "}"            { RBRACE }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "*"            { STAR }
  | "/"            { SLASH }
  | "%"            { PERCENT }
  | "=="           { EQ }
  | "!="           { NEQ }
  | "<"            { LT }
  | "<="           { LE }
  | ">"            { GT }
  | ">="           { GE }
  | "&&"           { AND }
  | "||"           { OR }
  | "!"            { NOT }
  | "&"            { BIT_AND }
  | "|"            { BIT_OR }
  | "^"            { BIT_XOR }
  | "~"            { BIT_NOT }
  | "="            { ASSIGN }
  | _              { raise_error "Unexpected character" lexbuf }
