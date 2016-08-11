type literal = 
    | Integer of int
    | Float of float

type expr = 
    | Paren of expr
    | Neg of expr
    | Mult of expr * expr
    | Div of expr * expr
    | Add of expr *  expr
    | Sub of expr * expr
    | Lit of literal

type types = Tint | Tfloat

type ast = 
    (* def type name exprs *)
    | Func of types * string * expr

let rep_literal = function
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x

let rec rep_expr = function
    | Lit(e1)       -> rep_literal e1
    | Add(e1, e2)   -> rep_expr e1 ^ " + " ^ rep_expr e2 
    | Sub(e1, e2)   -> rep_expr e1 ^ " - " ^ rep_expr e2
    | Mult(e1, e2)  -> rep_expr e1 ^ " * " ^ rep_expr e2
    | Div(e1, e2)   -> rep_expr e1 ^ " / " ^ rep_expr e2
    | Neg(e1)       -> "-" ^ rep_expr e1
    | Paren(e1)     -> "(" ^ rep_expr e1 ^ ")"

let rep_type = function
    | Tint      -> "int"
    | Tfloat    -> "float"

let rep_func = function
    | Func(tp, name, body)  -> "def " ^ (rep_type tp) ^ " " ^ name ^ "() = " ^ (rep_expr body) ^ ";"

let print_ast = List.iter (fun x -> print_endline(rep_func x))