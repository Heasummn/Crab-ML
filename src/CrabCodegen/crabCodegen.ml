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

let codegen_literal literal = match literal.data with 
    | Integer x     -> const_int int_type x
    | Float x       -> const_float float_type x

let store_var value alloca = ignore(build_store value alloca builder)

let create_var func name typ = let builder = builder_at context (instr_begin (entry_block func)) in
    build_alloca typ name builder

let rec code_neg ctx e1 =
    let expr = codegen_expr ctx e1 in
    let tp = e1.tp in
    let func = match tp with
        | TFloat    -> build_fneg
        | TInt      -> build_neg
        | _         -> assert false
    in
    func expr "negtmp" builder

and codegen_op ctx expr = 
    (* Code generation for arithmetic expressions *)
    let a_gen_op func_int func_f e1 e2 name =
        let expr1 = codegen_expr ctx e1 in let expr2 = codegen_expr ctx e2 in
        (* We assume that the Type Checking 
            has assured that these are the same type *)
        let func = match e1.tp with
            | TInt      -> func_int
            | TFloat    -> func_f
            | _         -> assert false
        in 
        (func expr1 expr2 name builder) 
    in
    
    let gen_builtin_op op e1 e2 = match op with 
            | "+"   -> a_gen_op build_add build_fadd e1 e2 "addtmp"
            | "-"   -> a_gen_op build_sub build_fsub e1 e2 "subtmp"
            | "*"   -> a_gen_op build_mul build_fmul e1 e2 "multmp"
            | "/"   -> a_gen_op build_sdiv build_fdiv e1 e2 "divtmp"
            | _ -> assert false
    in
    
    let gen_user_op op e1 e2 = 
        let callee = match lookup_function  ("__crab_func_" ^ op) glob_module with
            | Some callee   -> callee 
            | None          -> assert false
        in
        let args =  codegen_expr ctx e1 :: codegen_expr ctx e2 :: [] in
        let args = Array.of_list args in
        build_call callee args "calltmp" builder
    in

    let gen_helper = match expr.data with
        | BinOp(e1, op, e2) ->
                print_endline ("e1 has type " ^ rep_type e1.tp);
                let len =  List.length (List.filter (fun (_, (args, ret)) ->
                    args = [e1.tp; e2.tp] && ret = expr.tp) CrabEnv.ops) in
                if len >= 1 then
                    (* This is a built in operator *)
                    gen_builtin_op op e1 e2 
                else
                    gen_user_op op e1 e2

        | _                 -> assert false
    in

    match expr.data with
        | BinOp _ ->
            gen_helper
        | _ -> assert false
        
and codegen_expr ctx expr = 
    (* Make typing a bit easier *)
     match expr.data with 
    | Lit e1            -> codegen_literal e1
    | Paren e1          -> codegen_expr ctx e1
    | Neg e1            -> code_neg ctx e1
    | Var v1            -> Hashtbl.find named_vars v1
    | Assign a1         -> codegen_assign ctx a1
    | BinOp _           -> codegen_op ctx expr 

and codegen_assign ctx ((name, ty), expr, body) = 
    let ll_tp = type_to_llvm ty in
    let the_func = block_parent (insertion_block builder) in
        let alloca = create_var the_func name ll_tp in
        let value = codegen_expr ctx expr in
            store_var value alloca;
            Hashtbl.add named_vars name value;
    codegen_expr ctx body
    

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

let codegen_func ctx func = match func.data with 
    | Func(_, _, body)  ->
        (* Clear any old variables *)
        Hashtbl.clear named_vars;
        let the_function = codegen_proto func in
        let bb = append_block context "entry" the_function in
        position_at_end bb builder;
        try
            let ret_val = codegen_expr ctx body in
            ignore(build_ret ret_val builder);
            Llvm_analysis.assert_valid_function the_function;
            the_function
        with e ->
            delete_function the_function;
            raise e

let codegen_ast ctx tree =
    List.map (codegen_func ctx) tree
