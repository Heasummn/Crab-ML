open CrabCodegen
open CrabSemantic
open CrabCompile
open Batteries.String

let filename = Sys.argv.(1)
let output = fst (rsplit filename ".") ^ ".o"

let dump_funcs = List.iter (fun x -> dump_val x)

let main () = 
    let input = open_in filename in
    try
        let ctx = CrabEnv.base_ctx in
        let parsed = CrabParsing.process_chan input in
        let typed = annotateAST ctx parsed in
        ignore(codegen_ast ctx typed);
        init_compiler;
        create_obj output CrabCodegen.glob_module;
    with 
        | Error.SyntaxError (msg)   -> Printf.fprintf stderr "Syntax Error: %s\n" msg
        | Error.ParsingError(msg)   -> Printf.fprintf stderr "Parsing Error: %s\n" msg
        | Error.TypeError(msg)      -> Printf.fprintf stderr "Type Error: %s\n" msg
let _ = main ()
