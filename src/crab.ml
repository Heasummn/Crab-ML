open CrabCodegen
open CrabAst
open CrabTypeCheck

let filename = Sys.argv.(1)

let dump_funcs = List.iter (fun x -> dump_val x; print_endline "")

let main () = 
    let input = open_in filename in
    try
        let parsed = CrabParsing.process_chan input in
        print_ast parsed;
        print_endline "";
        typecheck parsed;
        dump_funcs (codegen_ast parsed)
    with 
        | Error.SyntaxError (msg)   -> Printf.fprintf stderr "Syntax Error: %s\n" msg
        | Error.ParsingError(msg)   -> Printf.fprintf stderr "Parsing Error: %s\n" msg
        | Error.TypeError(msg)            -> Printf.fprintf stderr "Type Error: %s\n" msg
let _ = main ()