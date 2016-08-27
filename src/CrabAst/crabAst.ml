(* If we want to add data to an AST node, we add it here *)

open Types

type ty = (string * tp)

type 'a annotation = { data: 'a; position: Location.t; tp: Types.tp }

type literal = simple_literal annotation
and simple_literal = 
    | Integer of int
    | Float of float

type expr = simple_expr annotation 
and simple_expr = 
    | Paren of expr
    | Neg of expr
    | BinOp of expr * string * expr
    | Lit of literal
    | Var of string
    | Assign of assign

and assign = ty * expr * expr 

type toplevel = simple_toplevel annotation
and simple_toplevel =
    (* def name args expr *)
    | Func of ty * ty list * expr


(* This works for all tp *)
let get_name = fst
let get_type = snd

let rep_literal lit = match lit.data with 
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x

let rec rep_expr expr = match expr.data with
    | Lit(e1)           -> rep_literal e1
    | BinOp(e1, op, e2) -> rep_expr e1 ^ " " ^ op ^ " " ^ rep_expr e2
    | Neg(e1)           -> "-" ^ rep_expr e1
    | Paren(e1)         -> "(" ^ rep_expr e1 ^ ")"
    | Var(v)            -> v
    | Assign(ass)       -> rep_assign ass

and rep_assign ((name, typ), value, body) = "let " ^ name ^ ": " ^ rep_type typ ^ 
    " = " ^ rep_expr value ^ " in " ^ rep_expr body


let rep_func func = match func.data with 
    | Func(def, args, body)  ->
        "def "  ^ get_name def ^ "(" ^
        (* Ugly hack *)
        (String.concat ", " (List.map rep_var args)) ^

        "): "^ (Types.rep_type (get_type def)) ^
        " = " ^ (rep_expr body) ^ ";"

let print_ast = List.iter (fun x -> print_endline(rep_func x))
