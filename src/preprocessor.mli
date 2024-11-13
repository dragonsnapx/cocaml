
type preprocessor_tokens =
	| IF
	| ELIF
	| ELSE
	| ENDIF
	| IFNDEF
	| IFDEF
	| DEFINE
	| UNDEF
	| INCLUDE

(** Preprocesses a file and returns the source file with the preprocessing complete *)
val preprocess_file : string -> string