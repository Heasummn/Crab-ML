{
  open Parser
  open Lexing

  exception SyntaxError of string

  let get_line lexbuf = 
  	let pos = lexbuf.lex_curr_p in
  		pos.pos_lnum

  let get_col lexbuf =
  	let pos = lexbuf.lex_curr_p in
  		pos.pos_cnum - pos.pos_bol
}

(* Values *)
let digit = ['0'-'9']
let int = digit+
let e = ['e' 'E']
let float = int '.' int e? '-'? int?

(* Whitespace *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| int			{ INT (int_of_string (Lexing.lexeme lexbuf)) }
	| float			{ FLOAT (float_of_string (Lexing.lexeme lexbuf)) }

	| whitespace	{ read lexbuf }
	| newline 		{ Lexing.new_line lexbuf; read lexbuf }
	| eof 			{ EOF }

	| '+'			{ PLUS }
	| '-'			{ MINUS }
	| '*'			{ MULT }
	| '/'			{ DIV }
	| '('			{ LPAREN }
	| ')'			{ RPAREN }
	| ';' 			{ SEMI }

	| "(-"			{ nested_comment 1 lexbuf }

	| _ 			{ raise (SyntaxError (
			"Unknown identifier: " ^ (Lexing.lexeme lexbuf) ^ 
			"\n    At line: " ^ (string_of_int (get_line lexbuf)) ^
			", Column: " ^ string_of_int (get_col lexbuf))) 
					}


and nested_comment level =
	parse
	| "(-"		{ nested_comment (level+1) lexbuf }
	| "-)"		{ if level = 1 then read lexbuf else nested_comment (level-1) lexbuf }
	| newline 	{ Lexing.new_line lexbuf; nested_comment level lexbuf }
	| _			{ nested_comment level lexbuf }