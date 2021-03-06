(* Inspired by the location system from Eff *)

type t = 
{
	filename: string;
	s_line: int;
	s_column: int;
	e_line: int;
	e_column: int;
}

let break pos = 
	let fname = pos.Lexing.pos_fname
	and line = pos.Lexing.pos_lnum
	and column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
	fname, line, column

let make start_pos end_pos = 
	let start_fname, s_line, s_column = break start_pos
	and _, e_line, e_column = break end_pos in
	{filename = start_fname; s_line; s_column; e_line; e_column }

let from_lex lexbuf = make (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let rep_position ?filename:(filename=false) pos = 
	(if filename then pos.filename ^ ": " else "") ^ "(" ^
		string_of_int pos.s_line ^ ", " ^ string_of_int pos.s_column ^ ") - (" ^
		string_of_int pos.e_line ^ ", " ^ string_of_int pos.e_column ^ ")"