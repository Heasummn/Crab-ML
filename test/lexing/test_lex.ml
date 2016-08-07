open OUnit2
open Lexer
open Lexing
open Parser
let extract token = match token with
	| Parser.INT(x) -> string_of_int x
	| SEMI			-> ";"
	| EOF 			-> "EOF"

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

let test_int_lex test_ctxt = 
	let lexbuf = Lexing.from_string "-42" in
	let lexed = lex lexbuf in
	let printer x = "[" ^ String.concat ", " x ^ "]" in
	assert_equal ~msg:"Integer Value" ~printer ["-42"; "EOF"] lexed

let test_program_lex test_ctxt = 
	let lexbuf = Lexing.from_string "004; -36; 		-087;" in
	let lexed = lex lexbuf in
	let printer x = "[" ^ String.concat ", " x ^ "]" in
	assert_equal ~msg:"Lex whole Program" ~printer ["4"; ";"; "-36"; ";"; "-87"; ";"; "EOF"] lexed

let suite = 
	"Lexing">:::
		["Test Lexing of Integers">::	test_int_lex;
		 "Text Lexing of program">::	test_program_lex
		];;

let () = run_test_tt_main suite