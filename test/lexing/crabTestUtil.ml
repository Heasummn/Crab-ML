open OUnit2
open Lexer
open Lexing
open Parser
open Batteries

let extract token = match token with
    | INT(x)    -> string_of_int x
    | FLOAT(x)  -> string_of_float x

    | PLUS      -> "+"
    | MINUS     -> "-"
    | MULT      -> "*"
    | DIV       -> "/"
    | LPAREN    -> "("
    | RPAREN    -> ")"      
    | SEMI      -> ";"
    | EOF       -> "EOF"

let lex lexbuf = 
    let rec recurse lexbuf tokens =
        let token = Lexer.read lexbuf in
        let tok_str = extract token in
        match token with
            | EOF   -> tok_str::tokens
            | _     -> recurse lexbuf (tok_str::tokens)
    in
    let rev_lex = recurse lexbuf [] in
    List.rev rev_lex

let assert_list ?msg:(msg="Value") xs ys = 
    let printer x = "[" ^ String.concat ", " x ^ "]" in
    assert_equal ~msg ~printer xs ys

let lex_from_string str = let lexbuf = Lexing.from_string str in
    lex lexbuf

let rec iter_files ?func:(func=fun x y -> ()) file1 file2 =
    let lines1 = File.lines_of file1 in let lines2 = File.lines_of file2 in
        Enum.iter2 func lines1 lines2

let comp_files input output =
    let compare in_line out_line = 
        assert_list
            (BatString.nsplit out_line ", ") (lex_from_string in_line) in
    iter_files input output ~func:compare