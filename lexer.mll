{
  open Parser

  exception SyntaxError of string
}

(* Values *)
let digit = ['0'-'9']
let int = '-'?digit+

(* Whitespace *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| int			{ INT (int_of_string (Lexing.lexeme lexbuf)) }
	| whitespace	{ read lexbuf }
	| newline 		{ Lexing.new_line lexbuf; read lexbuf }
	| eof 			{ EOF }

	| ';' 			{ SEMI }