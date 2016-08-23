open CrabAst
open Types
open Error
open Table

let check_func func args correct otherwise = 
    if (func args) then correct else raise(otherwise)

let check t1 t2 err = if (t1 == t2) then () else (raise(err))

(* The expression to check, and it's type. Expression given for nicer errors *)
let check_int e1 t1 = check_func (fun (x) -> x == TInt || x == TFloat) t1 () (SyntaxError(
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
        | Lit e1        -> let inferred = annotate_lit e1 in (Lit(inferred), inferred.tp)
        | Paren e1      -> let inferred = annotate_expr ctx e1 in (Paren(inferred), inferred.tp)
        | Neg _         -> let inferred = (a_unary_op ctx expr.data) in 
            (Neg(inferred), inferred.tp)
        | Var v1        -> let inferred = var_type ctx v1 in (Var(v1), inferred)
        (* This does not return the type of the variable, but rather the type of the body *)
        | Assign _     -> let value = assign_type ctx expr in (value.data, value.tp) 
        (* TODO: Refactor this *)
        | Add _         -> let inferred = (a_binary_op ctx expr.data) in
            Add(fst inferred, snd inferred), (fst inferred).tp;

        | Sub _         -> let inferred = (a_binary_op ctx expr.data) in
            Sub(fst inferred, snd inferred), (fst inferred).tp;
        | Mult _         -> let inferred = (a_binary_op ctx expr.data) in
            Mult(fst inferred, snd inferred), (fst inferred).tp;
        | Div _         -> let inferred = (a_binary_op ctx expr.data) in
            Div(fst inferred, snd inferred), (fst inferred).tp
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
        
        let ctx = Table.add (Symbol.symbol name) typ ctx in

        let inferred = annotate_expr ctx body in
        Assign((name, typ), value, inferred), typ
    | _     -> assert false
    
    in
    
    {   ass with data = fst annotated;
        tp = snd annotated;
    }

    

and a_unary_op ctx expr = match expr with
    | Neg e1        -> 
        let annotated = annotate_expr ctx e1 in
            check_int e1 annotated.tp;
            annotated
    | _             -> assert false

and a_binary_op ctx expr = 
    match expr with 
        | Add (e1, e2)
        | Sub (e1, e2)
        | Mult (e1, e2)
        | Div (e1, e2)      -> 
            let annotated1 = annotate_expr ctx e1 and
            annotated2 = annotate_expr ctx e2 in
                check_int e1 annotated1.tp;
                check_int e2 annotated2.tp;
                
                (* This is the first major choice within the language
                    A Binary operator, at least for now, must have the same type 
                    on both operands *)
                check annotated1.tp annotated2.tp (TypeError 
                    ("An Arithmetic Binary Operator requires that the " ^
                    "operands used within it are of the same type. But, " ^
                    (rep_type annotated1.tp) ^ " is not the same type as " ^
                    (rep_type annotated2.tp)));
                
                (
                    annotated1, annotated2
                )
        | _     -> assert false

and var_type ctx var = match lookup (Symbol.symbol var) ctx with
    | Some ty   -> ty
    | None      -> raise(TypeError("Unknown variable " ^ var))

let annotate_func func = match func.data with
    | Func(def, args, body)  ->
    let sym_args = List.map (fun (x,y) -> Symbol.symbol x, y) args in
    let ctx = Table.of_list sym_args in
    let inferred = annotate_expr ctx body in
        let tp = get_type (def)  in
        check tp inferred.tp (SyntaxError("In function " ^ get_name def ^ 
            ", expected type " ^ rep_type tp ^ ", but got type " ^ rep_type inferred.tp));
        
        {   func with data = Func(def, args, inferred);
            tp = (inferred).tp
        }
