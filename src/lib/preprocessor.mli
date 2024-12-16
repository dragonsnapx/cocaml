
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
val pp_file : string -> string

(** Define a preprocessor variable *)
val define_pp_variable : (string * string) -> unit

(** Check if a preprocessor variable is defined *)
val is_pp_variable_defined : string -> bool

(** Undefine a preprocessor variable *)
val undefine_pp_variable : string -> unit

(** #include, or paste a file in place of the #include directive. *)
val include_file : string -> string -> unit