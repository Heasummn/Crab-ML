open CrabCodegen
open CrabAst

let filename = Sys.argv.(1)

let rep_literal = function
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x

let rec rep_expr = function
    | Base(e1) | Lit(e1)       -> rep_literal e1
    | Add(e1, e2)   -> rep_expr e1 ^ " + " ^ rep_expr e2 
    | Sub(e1, e2)   -> rep_expr e1 ^ " - " ^ rep_expr e2
    | Mult(e1, e2)  -> rep_expr e1 ^ " * " ^ rep_expr e2
    | Div(e1, e2)   -> rep_expr e1 ^ " / " ^ rep_expr e2
    | Neg(e1)       -> "-" ^ rep_expr e1
    | Paren(e1)     -> "(" ^ rep_expr e1 ^ ")"


let print_ast (tree) = List.iter (fun x -> print_endline (rep_expr x)) tree

let dump_funcs = List.iter (fun x -> dump_val x; print_endline "")

let main () = 
    let input = open_in filename in
    try
        let parsed = CrabParsing.process_chan input in
        dump_funcs (codegen_ast parsed)
    with 
        | Error.SyntaxError (msg) -> Printf.fprintf stderr "Syntax Error: %s\n" msg
        | Error.ParsingError(msg) -> Printf.fprintf stderr "Parsing Error: %s\n" msg
let _ = main ()