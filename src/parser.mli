
type keywords_token =
	| AUTO
	| BREAK
	| CASE
	| CHAR of char
	| CONST
	| CONTINUE
	| DEFAULT
	| DO
	| DOUBLE
	| ELSE
	| ENUM
	| EXTERN
	| FLOAT
	| FOR
	| GOTO
	| IF
	| INT of int
	| LONG of int64
	| REGISTER
	| RETURN
	| SHORT
	| SIGNED
	| SIZEOF
	| STATIC
	| STRUCT
	| SWITCH
	| TYPEDEF
	| UNION
	| UNSIGNED
	| VOID
	| VOLATILE
	| WHILE

module Syntax_Node : sig
	type t
	val pos_start : int
  val pos_end : int
	val token_type : keywords_token
end

