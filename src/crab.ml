open! Core.Std
open Ast
open Codegen

let filename = Sys.argv.(1)
let parse lexbuf = Parser.program Lexer.read lexbuf

let rep_literal = function
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x

let rec rep_expr = function
    | Lit(e1)       -> rep_literal e1
    | Add(e1, e2)   -> rep_expr e1 ^ " + " ^ rep_expr e2 
    | Sub(e1, e2)   -> rep_expr e1 ^ " - " ^ rep_expr e2
    | Mult(e1, e2)  -> rep_expr e1 ^ " * " ^ rep_expr e2
    | Div(e1, e2)   -> rep_expr e1 ^ " / " ^ rep_expr e2
    | Neg(e1)       -> "-" ^ rep_expr e1
    | Paren(e1)     -> "(" ^ rep_expr e1 ^ ")"


let print_ast (tree) = List.iter ~f:(fun x -> print_endline (rep_expr x)) tree

let main () = 
    let input = open_in filename in
    try
        let parsed = Parse.process_chan input in
        List.iter ~f:(dump_val) (codegen_ast parsed)
    with 
        | Lexer.SyntaxError (msg) -> Printf.fprintf stderr "Syntax Error: %s\n" msg
        | Parse.ParsingError(msg) -> Printf.fprintf stderr "Parsing Error: %s\n" msg
let _ = main ()