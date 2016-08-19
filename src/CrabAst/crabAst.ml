(* If we want to add data to an AST node, we add it here *)

open Types

type 'a annotation = { data: 'a; position: Location.t; tp: Types.tp }

type literal = simple_literal annotation
and simple_literal = 
    | Integer of int
    | Float of float

type expr = simple_expr annotation 
and simple_expr = 
    | Paren of expr
    | Neg of expr
    | Mult of expr * expr
    | Div of expr * expr
    | Add of expr *  expr
    | Sub of expr * expr
    | Lit of literal
    | Var of string

type toplevel = simple_toplevel annotation
and simple_toplevel =
    (* def name args expr *)
    | Func of ty * ty list * expr
and ty = (string * tp)

let get_name = fst
let get_type = snd

let rep_literal lit = match lit.data with 
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x

let rec rep_expr expr = match expr.data with
    | Lit(e1)       -> rep_literal e1
    | Add(e1, e2)   -> rep_expr e1 ^ " + " ^ rep_expr e2 
    | Sub(e1, e2)   -> rep_expr e1 ^ " - " ^ rep_expr e2
    | Mult(e1, e2)  -> rep_expr e1 ^ " * " ^ rep_expr e2
    | Div(e1, e2)   -> rep_expr e1 ^ " / " ^ rep_expr e2
    | Neg(e1)       -> "-" ^ rep_expr e1
    | Paren(e1)     -> "(" ^ rep_expr e1 ^ ")"
    | Var(v)        -> v

let rep_func func = match func.data with 
    | Func(def, args, body)  ->
        "def "  ^ get_name def ^ "(" ^
        (* Ugly hack *)
        (String.concat ", " (List.map rep_var args)) ^

        "): "^ (Types.rep_type (get_type def)) ^
        " = " ^ (rep_expr body) ^ ";"

let print_ast = List.iter (fun x -> print_endline(rep_func x))