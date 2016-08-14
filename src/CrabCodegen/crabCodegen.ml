open Llvm
open CrabAst
open Types

let context = global_context ()
let glob_module = create_module context "Crab"
let builder = builder context

let dump_val = dump_value
let dump_mod = dump_module

(* TODO: Replace this with a Hash Table *)
let int_type = integer_type context 64
let float_type = double_type context
let void = void_type context

let codegen_literal literal = match literal.data with 
    | Integer x     -> const_int int_type x
    | Float x       -> const_float float_type x

let rec codegen_expr expr = match expr.data with 
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

let type_to_llvm = function
    | TEmpty -> void
    | TInt   -> int_type
    | TFloat -> float_type

(* Later, use the type checked type of the function instead of given *)
let codegen_proto func = match func.data with
    | Func(_, name, _)  -> let void = Array.make 0 void in
        let ft = function_type (type_to_llvm func.tp) void in
        declare_function name ft glob_module

let codegen_func func = match func.data with 
    | Func(_, _, body)  ->
        let the_function = codegen_proto func in
        let bb = append_block context "entry" the_function in
        position_at_end bb builder;
        try
            let ret_val =   codegen_expr body in
            ignore(build_ret ret_val builder);
            Llvm_analysis.assert_valid_function the_function;

            the_function
        with e ->
            delete_function the_function;
            raise e

let codegen_ast tree =
    List.map codegen_func tree