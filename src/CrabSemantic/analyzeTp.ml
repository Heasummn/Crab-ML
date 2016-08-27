open CrabAst
open Types
open Error
open Table
open CrabEnv

let func_context = ref base_ctx

let check_func func args correct otherwise = 
    if (func args) then correct else raise(otherwise)

let check t1 t2 err = if (t1 == t2) then () else (raise(err))

(* The expression to check, and it's type. Expression given for nicer errors *)
let check_int e1 t1 = check_func (fun (x) -> x == TInt || x == TFloat) t1 () (TypeError(
    "In expression " ^ rep_expr e1 ^ ", expected numeric type, but instead got " ^ 
    rep_type t1))

let annotate_lit lit =
    let inferred =  match lit.data with
        | Integer _         -> TInt        
        | Float _           -> TFloat
    in
    { lit with tp = inferred }

let rec annotate_expr ctx expr =
    let typed = match expr.data with
        (* Return the updated data, and the type -> (data, type) *)
        | Lit e1            -> let inferred = annotate_lit e1 in (Lit(inferred), inferred.tp)
        | Paren e1          -> let inferred = annotate_expr ctx e1 in (Paren(inferred), inferred.tp)
        | Neg _             -> let inferred = (a_unary_op ctx expr.data) in 
            (Neg(inferred), inferred.tp)
        | Var v1            -> let inferred = var_type ctx v1 in (Var(v1), inferred)
        (* This does not return the type of the variable, but rather the type of the body *)
        | Assign _          -> let value = assign_type ctx expr in (value.data, value.tp)
        (* op_type returns a tuple of the updated data, and the inferred type  *)
        | BinOp _           -> let (data, inferred) = op_type ctx expr in 
            (data, inferred)
    in
    {   expr with data = fst typed;
        tp = snd typed
    }

(* Update the variable, and return the type of body *)
and assign_type ctx ass =
    let annotated = match ass.data with 
    | Assign((name, typ), value, body) ->
        let t_val = annotate_expr ctx value in
        check typ t_val.tp (TypeError ("Variable " ^ name ^ " is expected to have type " ^ rep_type typ 
        ^ ", but has type " ^ rep_type t_val.tp));
        
        let ctx = { ctx with vars = Table.add (Symbol.symbol name) typ ctx.vars } in

        let inferred = annotate_expr ctx body in
        Assign((name, typ), value, inferred), typ
    | _     -> assert false
    
    in
    
    {   ass with data = fst annotated;
        tp = snd annotated;
    }

and op_type ctx expr = 
    (* Lookup the operator in the context, assert that it is used correctly, and then return it's return type *)
    match expr.data with
        | BinOp(e1, op, e2) ->
                let choices = match MultiTable.lookup (Symbol.symbol op) !func_context.ops with
                    | Some x    -> x
                    | None      -> (raise(TypeError("Unknown operator " ^ op)))
                in
                    (* We assume that the creation of the operator already checks if it is valid *)
                    (* Therefore, Filtering it should return only one value. *)
                    let e1 = annotate_expr ctx e1 and e2 = annotate_expr ctx e2 in
                    let set = (BatSet.filter (fun (args, _) ->
                        try
                            (* Assert that the args are equal *)
                            args = [e1.tp; e2.tp]
                        with
                            | Failure _     -> false    
                        ) choices) in
                    let func = 
                        try
                            snd(fst (BatSet.pop set))
                        with
                            | Not_found     -> (raise (
                                TypeError("Operator " ^ op ^ " is used as if it had type " ^ rep_type e1.tp ^
                            " and " ^ rep_type e2.tp ^ ", but no such operator has been found.")
                            ))
                        in
                    (BinOp(e1, op, e2), func)
                    
        | _             -> assert false
        


and a_unary_op ctx expr = match expr with
    | Neg e1        -> 
        let annotated = annotate_expr ctx e1 in
            check_int e1 annotated.tp;
            annotated
    | _             -> assert false


and var_type ctx var = match lookup (Symbol.symbol var) ctx.vars with
    | Some ty   -> ty
    | None      -> raise(TypeError("Unknown variable " ^ var))

let annotate_func ctx func = match func.data with
    | Func(def, args, body)  ->
    let sym_args = List.map (fun (x,y) -> Symbol.symbol x, y) args in
    
    (* An ugly hack to join the two ctx's, TODO: Make a function *)
    let ctx = { ctx with vars = Table.of_enum (BatEnum.append (Table.enum ctx.vars) (Batteries.List.enum sym_args)) } in
    

    let inferred = annotate_expr ctx body in
        let tp = get_type (def)  in
        check tp inferred.tp (TypeError("In function " ^ get_name def ^ 
            ", expected type " ^ rep_type tp ^ ", but got type " ^ rep_type inferred.tp));
        
        {   func with data = Func(def, args, inferred);
            tp = (inferred).tp
        }
    
    | Operator((name, ty), args, body) ->
        let sym_name = Symbol.symbol name in
        let types = List.map snd args in
        let len = List.length args in
        if len > 2 then
            (raise(TypeError("Operator " ^ name ^ " must have at most 2 arguments, but it has " ^ 
            string_of_int len ^ " arguments.")));
            
        let sym_args = List.map (fun (x,y) -> Symbol.symbol x, y) args in
    
        (* An ugly hack to join the two ctx's, TODO: Make a function *)
        let ctx = { ctx with vars = Table.of_enum (BatEnum.append (Table.enum ctx.vars) (Batteries.List.enum sym_args)) } in

        let ops = MultiTable.add sym_name (types, ty) ctx.ops in
        func_context := { ctx with ops };
        let inferred = annotate_expr ctx body in
            check ty inferred.tp (TypeError("In operator " ^ name ^ 
                ", expected type " ^ rep_type ty ^ ", but got type " ^ rep_type inferred.tp));
        
            {   func with data = Operator((name, ty), args, inferred);
                tp = (inferred).tp
            }
        
