open Lexing

exception SyntaxError of string
exception ParsingError of string
exception TypeError of string

let get_line lexbuf = 
	let pos = lexeme_start_p lexbuf in
		pos.pos_lnum

let get_start_col lexbuf =
let pos = lexeme_start_p lexbuf in
	pos.pos_cnum - pos.pos_bol + 1

let get_end_col lexbuf = 
	let pos = lexeme_end_p lexbuf in
	pos.pos_cnum - pos.pos_bol + 1