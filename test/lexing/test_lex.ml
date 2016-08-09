open OUnit2
open Lexer
open Lexing
open Parser

let extract token = match token with
	| INT(x) 	-> string_of_int x
	| FLOAT(x)	-> string_of_float x

	| PLUS		-> "+"
	| MINUS	 	-> "-"
	| MULT	 	-> "*"
	| DIV	 	-> "/"
	| LPAREN 	-> "("
	| RPAREN	-> ")"		
	| SEMI		-> ";"
	| EOF 		-> "EOF"

let lex lexbuf = 
	let rec recurse lexbuf tokens =
		let token = Lexer.read lexbuf in
		let tok_str = extract token in
		match token with
			| EOF	-> tok_str::tokens
			| _ 	-> recurse lexbuf (tok_str::tokens)
	in
	let rev_lex = recurse lexbuf [] in
	List.rev rev_lex

let assert_list ?msg:(msg="Value") xs ys = 
	let printer x = "[" ^ String.concat ", " x ^ "]" in
	assert_equal ~msg ~printer xs ys

let lex_from_string str = let lexbuf = Lexing.from_string str in
	lex lexbuf

let test_int_lex test_ctxt = 
	let lexed = lex_from_string "-42" in
	assert_list ~msg:"Integer Value" ["-"; "42"; "EOF"] lexed

let test_program_lex test_ctxt = 
	let lexed = lex_from_string "004; -36; 		-087;" in
	assert_list ~msg:"Lex whole program" ["4"; ";"; "-"; "36"; ";"; "-"; "87"; ";"; "EOF"] lexed

let suite = 
	"Lexing">:::
		["Test Lexing of Integers">::	test_int_lex;
		 "Text Lexing of program">::	test_program_lex
		];;

let () = run_test_tt_main suite