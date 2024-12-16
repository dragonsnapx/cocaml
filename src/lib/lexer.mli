(** Read the input buffer and return the next token. *)
val token : Lexing.lexbuf -> Menhir_parser.token

(** Exception for Lexer errors. *)
exception LexerError of string * Lexing.position
