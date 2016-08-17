open CrabAst
open Types
open Error
open CrabEnv

let vars = base_var_env

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

let rec annotate_expr expr =
    let typed = match expr.data with
        (* Return the updated data, and the type -> (data, type) *)
        | Lit e1        -> let inferred = annotate_lit e1 in (Lit(inferred), inferred.tp)
        | Paren e1      -> let inferred = annotate_expr e1 in (Paren(inferred), inferred.tp)
        | Neg _         -> let inferred = (a_unary_op expr.data) in 
            (Neg(inferred), inferred.tp)

        (* TODO: Refactor this *)
        | Add _         -> let inferred = (a_binary_op expr.data) in
            (Add(fst inferred, snd inferred), (fst inferred).tp)
        | Sub _         -> let inferred = (a_binary_op expr.data) in
            (Sub(fst inferred, snd inferred), (fst inferred).tp)
        | Mult _         -> let inferred = (a_binary_op expr.data) in
            (Mult(fst inferred, snd inferred), (fst inferred).tp)
        | Div _         -> let inferred = (a_binary_op expr.data) in
            (Div(fst inferred, snd inferred), (fst inferred).tp)    in
    {   expr with data = fst typed;
        tp = snd typed
    }

and a_unary_op expr = match expr with
    | Neg e1        -> 
        let annotated = annotate_expr e1 in
            check_int e1 annotated.tp;
            annotated
    | _             -> assert false

and a_binary_op expr = 
    let tightest_bind t1 t2 = match t1, t2 with 
        | (TFloat, _)   -> TFloat
        | (_, TFloat)   -> TFloat
        | (TInt, _)     -> TInt        | _             -> assert false
    in

    match expr with 
        | Add (e1, e2)
        | Sub (e1, e2)
        | Mult (e1, e2)
        | Div (e1, e2)      -> 
            let annotated1 = annotate_expr e1 and
            annotated2 = annotate_expr e2 in
                check_int e1 annotated1.tp;
                check_int e2 annotated2.tp;
                let tp = tightest_bind annotated1.tp annotated2.tp
                in
                (
                    {annotated1 with tp}, {annotated2 with tp}
                )
        | _     -> assert false


let annotate_func func = match func.data with
    | Func(def, args, body)  -> 

    let inferred = annotate_expr body in
        let tp = get_type def and name = get_name def in
        check tp inferred.tp (SyntaxError("In function " ^ name ^ 
            ", expected type " ^ rep_type tp ^ ", but got type " ^ rep_type inferred.tp));
        {   func with data = Func(def, args, inferred);
            tp = (inferred).tp
        }