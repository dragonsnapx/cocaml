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

  (* Helper function for accumulating a string's content, throwing any needed errors *)
  let rec parse_string acc lexbuf =
    match Lexing.lexeme lexbuf with
      | "\"" -> 
        (* Case 1: Closing quote *)
        STRING_LITERAL (String.concat "" (List.rev acc))
      | "\\" -> 
        (* Case 2: Escape character *)
        let next_char = Lexing.lexeme_char lexbuf 1 in
        let decoded =
          match next_char with
            | '"'  -> "\""  
            | '\\' -> "\\"   
            | 'n'  -> "\n"   
            | 't'  -> "\t"  
            | 'r'  -> "\r"   
            | '0'  -> "\000" 
            | _ -> raise_error "Invalid escape sequence" lexbuf
          in
        Lexing.new_line lexbuf;
        parse_string (decoded :: acc) lexbuf
      | _ ->
        (* Case 3: Valid character *)
        let char = Lexing.lexeme lexbuf in
        parse_string (char :: acc) lexbuf
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
  | '"' { parse_string [] lexbuf } 

  (* Multi-Line Comments *)
  | "/*" { multi_line_comment lexbuf; token lexbuf }

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
  | whitespace+    { token lexbuf } (* Ignore whitespace *)
  | newline        { token lexbuf } (* Ignore newlines *)
  | comment_single { token lexbuf } (* Ignore single-line comments *)
  | eof            { EOF }          (* End of file *)
  | _              { raise_error "Unexpected character" lexbuf }

and multi_line_comment = parse
  | "*/" { () }                                                 (* Properly closed comment *)
  | eof  { raise_error "Unclosed multi-line comment" lexbuf }
  | [^'*']+ { multi_line_comment lexbuf }                       (* Consume non-* characters *)
  | '*' [^'/'] { multi_line_comment lexbuf }                    (* Consume '*' not followed by '/' *)
  | _ { multi_line_comment lexbuf }                             (* Consume remaining characters *)