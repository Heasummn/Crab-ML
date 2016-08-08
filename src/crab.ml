open Core.Std
open Ast
open Lexer

let filename = Sys.argv.(1)

let rep_literal = function
	| Integer(x) 	-> string_of_int x
	| Float(x)		-> string_of_float x 

let rec rep_expr = function
	| Lit(e1)		-> rep_literal e1
	| Add(e1, e2)	-> rep_expr e1 ^ " + " ^ rep_expr e2 
	| Sub(e1, e2)	-> rep_expr e1 ^ " - " ^ rep_expr e2
	| Neg(e1)		-> "-" ^ rep_expr e1

let parse lexbuf = Parser.program Lexer.read lexbuf

let print_ast (tree) = List.iter ~f:(fun x -> print_endline (rep_expr x)) tree

let main () = 
	let input = open_in filename in
	let lexbuf = Lexing.from_channel input in
	try
		print_ast (parse lexbuf)
	with 
		| SyntaxError (msg) -> print_endline ("Error: " ^ msg)
let _ = main ()