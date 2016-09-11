open CrabCodegen
open CrabSemantic
open CrabCompile
open CrabPasses
open Batteries.String

let exit_status = ref 0

let filename = Sys.argv.(1)
let output = fst (rsplit filename ".") ^ ".o"

let dump_funcs = List.iter (fun x -> dump_val x)

let main () = 
    let input = open_in filename in
    begin
        try
        let ctx = CrabEnv.base_ctx in
        let parsed = CrabParsing.process_chan input in

        let first_pass = stage1 ctx parsed in
        let second_pass = stage2 ctx first_pass in
        let annotated = annotateAST ctx second_pass in
        let third_pass = stage3 ctx annotated in

        ignore(codegen_ast ctx third_pass);
        init_compiler;
        create_obj output CrabCodegen.glob_module;
    with 
        | Error.SyntaxError (msg)   -> Printf.fprintf stderr "Syntax Error: %s\n" msg; exit_status := 1;
        | Error.ParsingError(msg)   -> Printf.fprintf stderr "Parsing Error: %s\n" msg; exit_status := 1;
        | Error.TypeError(msg)      -> Printf.fprintf stderr "Type Error: %s\n" msg; exit_status := 1;
    end;
    exit !exit_status
let _ = main ()
