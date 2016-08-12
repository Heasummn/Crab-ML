type t = 
{
	filename: string;
	s_line: int;
	s_column: int;
	e_line: int;
	e_column: int;
}

(* Create a location using a start and end position *)
val make		: Lexing.position -> Lexing.position -> t
val from_lex	: Lexing.lexbuf -> t
val rep_position: ?filename:bool -> t -> string