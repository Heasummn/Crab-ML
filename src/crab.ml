open Core.Std
open Ast
open Lexer

let filename = Sys.argv.(1)

let print_expr (expr) = match expr with
	| Integer(x)	-> print_endline ( string_of_int x )
	| Float(x)		-> print_endline ( string_of_float x ) 

let parse lexbuf = Parser.program Lexer.read lexbuf

let print_ast (tree) = List.iter ~f:(print_expr) tree

let main () = 
	let input = open_in filename in
	let lexbuf = Lexing.from_channel input in
	try
		print_ast (parse lexbuf)
	with 
		| SyntaxError (msg) -> print_endline ("Error: " ^ msg)
let _ = main ()