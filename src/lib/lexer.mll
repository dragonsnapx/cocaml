{
  open Menhir_parser
  open Lexing

  exception LexerError of string * Lexing.position

  (* Helper function to raise a detailed lexer error *)
  let raise_error msg pos =
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    raise (LexerError (Printf.sprintf "Line %d, column %d: %s" line col msg, pos))
}

(* Lexer rules *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'
let valid_char = ['\032'-'\033' '\035'-'\091' '\093'-'\126']
let escaped_char = '\\' ['"' '\\' 'n' 't' 'r' '0']
let comment_single = "//" [^'\n']* ('\n' | eof)

rule token = parse
  (* Strings *)
  | '"' { parse_string [] lexbuf.lex_start_p lexbuf } 

  (* Multi-Line Comments *)
  | "/*" { parse_multiline_comment lexbuf.lex_start_p lexbuf; token lexbuf }

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
  | "+="           { PLUS_EQUAL }
  | "-="           { MINUS_EQUAL }
  | "*="           { STAR_EQUAL }
  | "/="           { SLASH_EQUAL }
  | "%="           { PERCENT_EQUAL }
  | "++"           { INCREMENT }
  | "--"           { DECREMENT }
  | "<<"           { LEFT_SHIFT }
  | ">>"           { RIGHT_SHIFT } 
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
  | whitespace+    { token lexbuf }                                                          (* Whitespace *)
  | newline        { Lexing.new_line lexbuf; token lexbuf }                                  (* Newline *)
  | comment_single { Lexing.new_line lexbuf; token lexbuf }                                  (* Single-line comment *)
  | eof            { EOF }                                                                   (* End of file *)
  | _              { raise_error "Unexpected character" lexbuf.lex_start_p }

and parse_string acc start_pos = parse
  | "\""           { STRING_LITERAL (String.concat "" (List.rev acc)) }
  | "\\"           { let decoded =
                      match Lexing.lexeme_char lexbuf 1 with
                        | '"' -> "\""
                        | '\\' -> "\\"
                        | 'n'  -> "\n"   
                        | 't'  -> "\t" 
                        | 'r'  -> "\r"   
                        | '0'  -> "\000" 
                        | _    -> raise_error "Invalid escape sequence" lexbuf.lex_start_p
                      in
                      parse_string (decoded :: acc) start_pos lexbuf
                   }
  | eof            { raise_error "Unclosed string literal" start_pos }
  | _              { let char = Lexing.lexeme lexbuf in
                     parse_string (char :: acc) start_pos lexbuf
  }

and parse_multiline_comment start_pos = parse
  | "*/"           { () }                                                                        (* Properly closed comment *)
  | eof            { raise_error "Unclosed comment" start_pos}
  | '\n'           { Lexing.new_line lexbuf; parse_multiline_comment start_pos lexbuf }
  | [^'*']+        { parse_multiline_comment start_pos lexbuf }                                  (* Consume non-* characters *)
  | '*' [^'/']     { parse_multiline_comment start_pos lexbuf }                                  (* Consume '*' not followed by '/' *)
  | _              { parse_multiline_comment start_pos lexbuf }                                  (* Consume remaining characters *)

