open Llvm
open CrabAst
open Types

let context = global_context ()
let glob_module = create_module context "Crab"
let builder = builder context

let named_vars: (string, llvalue) Hashtbl.t = Hashtbl.create 15

let dump_val = dump_value
let dump_mod = dump_module

(* TODO: Replace this with a Hash Table *)
let int_type = integer_type context 64
let float_type = double_type context
let void = void_type context

let type_to_llvm = function
    | TEmpty -> void
    | TInt   -> int_type
    | TFloat -> float_type

let llvm_to_type = function
    | TypeKind.Integer  -> TInt
    | TypeKind.Double   -> TFloat
    | TypeKind.Void     -> TEmpty
    | _                 -> assert false

let codegen_literal literal = match literal.data with 
    | Integer x     -> const_int int_type x
    | Float x       -> const_float float_type x

let rec code_neg e1 =
    let expr = codegen_expr e1 in
    let tp = llvm_to_type (classify_type (type_of expr)) in
    let func = match tp with
        | TFloat    -> build_fneg
        | TInt      -> build_neg
        | _         -> assert false
    in
    func expr "negtmp" builder

and a_codegen_op func_int func_f e1 e2 name =
    let expr1 = codegen_expr e1 in let expr2 = codegen_expr e2 in
        (* We assume that the Type Checking 
            has assured that these are the same type *)
    let func = match llvm_to_type (classify_type (type_of expr1)) with
        | TInt      -> func_int
        | TFloat    -> func_f
        | _         -> assert false
    in 
    (func expr1 expr2 name builder)


and codegen_expr expr = 
    (* Make typing a bit easier *)
    let gen_op func_int func_f e1 e2 name = 
        a_codegen_op func_int func_f e1 e2 name
    in
     match expr.data with 
    | Lit e1        -> codegen_literal e1
    | Paren e1      -> codegen_expr e1
    | Neg e1        -> code_neg e1
    | Var v1        -> Hashtbl.find named_vars v1
    | Add (e1, e2)  -> gen_op build_add build_fadd e1 e2 "addtmp"
    | Sub (e1, e2)  -> gen_op build_sub build_fsub e1 e2 "subtmp"
    | Mult (e1, e2) -> gen_op build_mul build_fmul e1 e2 "multmp"
    | Div (e1, e2)  -> gen_op build_sdiv build_fdiv e1 e2 "divtmp"

let codegen_proto func = match func.data with
    | Func(def, args, _)  -> 
        let arg_array = Array.of_list args in
        let arg_type = Array.map (fun x -> type_to_llvm (get_type x)) arg_array
        and name = get_name def in
        let ft = function_type (type_to_llvm func.tp) arg_type in
        let f = declare_function name ft glob_module in

        (* Add variables to named values and register with LLVM *)
        Array.iteri (fun i a ->
            let n = get_name (arg_array.(i)) in
            Hashtbl.add named_vars n a;
            set_value_name n a;
        ) (params f);
        f

let codegen_func func = match func.data with 
    | Func(_, _, body)  ->
        (* Clear any old variables *)
        Hashtbl.clear named_vars;
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