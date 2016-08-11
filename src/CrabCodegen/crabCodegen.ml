open Llvm
open CrabAst

let context = global_context ()
let glob_module = create_module context "Crab"
let builder = builder context

let dump_val = dump_value
let dump_mod = dump_module

(* TODO: Replace this with a Hash Table *)
let int_type = integer_type context 64
let float_type = double_type context

let codegen_literal = function
    | Integer x     -> const_int int_type x
    | Float x       -> const_float float_type x

let rec codegen_expr = function
    | Lit e1        -> codegen_literal e1
    | Paren e1      -> codegen_expr e1
    | Neg e1        -> build_fneg (codegen_expr e1) "negtmp" builder

    | Add (e1, e2)  -> 
        let e1_val = codegen_expr e1 in let e2_val = codegen_expr e2 in
            build_fadd e1_val e2_val "addtmp" builder
    | Sub (e1, e2)  -> 
        let e1_val = codegen_expr e1 in let e2_val = codegen_expr e2 in
            build_fsub e1_val e2_val "subtmp" builder
    | Mult (e1, e2) -> 
        let e1_val = codegen_expr e1 in let e2_val = codegen_expr e2 in
            build_fmul e1_val e2_val "multmp" builder
    | Div (e1, e2)  -> 
        let e1_val = codegen_expr e1 in let e2_val = codegen_expr e2 in
            build_fdiv e1_val e2_val "divtmp" builder

let codegen_func = function 
    | Func(_, _, body)  -> codegen_expr body

let codegen_ast tree =
    List.map codegen_func tree