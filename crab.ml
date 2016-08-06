open Ast

let filename = Sys.argv.(1)

let print_expr expr = match expr with
| Integer(x) -> print_endline ( string_of_int x )

let print_ast ast = List.iter (print_expr) ast  


let main () = 
	let input = open_in filename in
	let file = Lexing.from_channel input in
		print_ast (Parser.program Lexer.read file)

let _ = main ()