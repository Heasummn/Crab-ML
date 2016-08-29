open CrabCodegen
open CrabAst
open CrabSemantic
open CrabCompile

let filename = Sys.argv.(1)

let dump_funcs = List.iter (fun x -> dump_val x)

let main () = 
    let input = open_in filename in
    try
        let ctx = CrabEnv.base_ctx in
        let parsed = CrabParsing.process_chan input in
        print_ast parsed;
        let typed = annotateAST ctx parsed in
        ignore(codegen_ast ctx typed);
        init;
        create CrabCodegen.glob_module;
    with 
        | Error.SyntaxError (msg)   -> Printf.fprintf stderr "Syntax Error: %s\n" msg
        | Error.ParsingError(msg)   -> Printf.fprintf stderr "Parsing Error: %s\n" msg
        | Error.TypeError(msg)      -> Printf.fprintf stderr "Type Error: %s\n" msg
let _ = main ()
