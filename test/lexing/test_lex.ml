open CrabTestUtil
open OUnit2

let test_int_program_lex test_ctxt = 
	let input = "test/lexing/input/integers.cb" in
	let output = "test/lexing/output/integers.cb" in
	comp_files input output

let test_float_program_lex test_ctxt = 
	let input = "test/lexing/input/floats.cb" in
	let output = "test/lexing/output/floats.cb" in
	comp_files input output

let suite = 
	"Lexing">:::
		["Text Lexing of Integer program">::			test_int_program_lex;
		 "Test Lexing of program composed of Floats">:: test_float_program_lex
		];; 

let () = run_test_tt_main suite