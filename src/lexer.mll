{
  open Menhir_parser
  open Lexing

  exception LexerError of string * Lexing.position

  (* Helper function to raise a detailed lexer error *)
  let raise_error msg lexbuf =
    let pos = lexbuf.lex_start_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    raise (LexerError (Printf.sprintf "Line %d, column %d: %s" line col msg, pos))

  (* Helper function for parsing strings and verifying they are closed *)
  let parse_string lexbuf =
    let rec parse_string_aux () =
      match%sedlex lexbuf with
      | '"' -> ()                                           (* Properly closed string *)
      | '\\' -> (match%sedlex lexbuf with
                 | _ -> parse_string_aux ())                (* Handle escape sequences *)
      | eof -> raise_error "Unclosed string literal" lexbuf
      | _ -> parse_string_aux ()                            (* Consume valid characters *)
    in
    parse_string_aux ();
    STRING_LITERAL (Lexing.lexeme lexbuf)

  (* Helper function for parsing multi-line comments and verifying they are closed *)
  let parse_multi_line_comment lexbuf =
    let rec parse_multi_line_comment_aux () =
      match%sedlex lexbuf with
      | "*/" -> ()                                          (* Properly closed comment *)
      | eof -> raise_error "Unclosed comment" lexbuf
      | _ -> parse_multi_line_comment_aux ()                (* Consume valid characters *)
    in
    parse_multi_line_comment_aux ()
}

(* Lexer rules *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let string_content = '"' ([^'"' '\\'] | '\\' .)* '"'
let comment_single = "//" [^'\n']* ('\n' | eof)
let comment_multi = "/*" ([^'*'] | ('*' [^'/']))* "*/"


rule token = parse
  (* Strings *)
  | '"' { parse_string lexbuf }

  (* Multi-Line Comments *)
  | "/*" { parse_multi_line_comment lexbuf; token lexbuf }

  (* Keywords and identifiers *)
  | "int"          { INT }
  | "float"        { FLOAT }
  | "char"         { CHAR }
  | "long"         { LONG }
  | "double"       { DOUBLE }
  | "static"       { STATIC }
  | "void"         { VOID }
  | "return"       { RETURN }
  | "if"           { IF }
  | "else"         { ELSE }
  | "for"          { FOR }
  | "while"        { WHILE }
  | "do"           { DO }
  | "switch"       { SWITCH }
  | "case"         { CASE }
  | "default"      { DEFAULT }
  | "break"        { BREAK }
  | "struct"       { STRUCT }
  | "typedef"      { TYPEDEF }

  (* Literals and Operators *)
  | digit+         { INT_LITERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | digit+ '.' digit+ { FLOAT_LITERAL (float_of_string (Lexing.lexeme lexbuf)) }
  | '\'' [^'\''] '\'' { CHAR_LITERAL (Lexing.lexeme lexbuf).[1] }   
  | identifier     { IDENT (Lexing.lexeme lexbuf) }
  | "=="           { EQUAL }
  | "!="           { NOT_EQUAL }
  | "<"            { LESS_THAN }
  | "<="           { LESS_EQUAL }
  | ">"            { GREATER_THAN }
  | ">="           { GREATER_EQUAL }
  | "&&"           { AND }
  | "||"           { OR }
  | "!"            { NOT }
  | "&"            { AMPERSAND }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "{"            { LBRACE }
  | "}"            { RBRACE }
  | ';'            { SEMI }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "*"            { STAR }
  | "/"            { SLASH }
  | "%"            { PERCENT }
  | "!"            { NOT }
  | "&"            { AMPERSAND }
  | "|"            { BIT_OR }
  | "^"            { BIT_XOR }
  | "~"            { BIT_NOT }
  | "="            { ASSIGN }
  | whitespace+    { token lexbuf } (* Ignore whitespace *)
  | newline        { token lexbuf } (* Ignore newlines *)
  | comment_single { token lexbuf } (* Ignore single-line comments *)
  | eof            { EOF }          (* End of file *)
  | _              { raise_error "Unexpected character" lexbuf }
