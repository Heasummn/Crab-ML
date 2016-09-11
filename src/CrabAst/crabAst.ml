(* If we want to add data to an AST node, we add it here *)

open Types

type ty = (string * tp)

type 'a annotation = { data: 'a; position: Location.t; tp: Types.tp }

type literal = simple_literal annotation
and simple_literal = 
    | Integer of int
    | Float of float
    | Bool of bool

type expr = simple_expr annotation 
and simple_expr = 
    | Paren of expr
    | Neg of expr
    | BinOp of expr * string * expr
    | Lit of literal
    | Call of string * expr list
    | Var of string
    | Assign of assign

and assign = ty * expr * expr 

type toplevel = simple_toplevel annotation
and simple_toplevel =
    (* def name args expr *)
    | Func of ty * (string list * tp) * expr
    | Operator of ty * (string list * tp)  * expr
    | Extern of ty * (string list * tp)


let conv_args args = 
    let arg_list = fst args in
    let types = arrow_list (snd args) in
    List.combine arg_list types
    

(* This works for all tp *)
let get_name = fst
let get_type = snd

let rep_literal lit = match lit.data with 
    | Integer(x)    -> string_of_int x
    | Float(x)      -> string_of_float x
    | Bool(x)       -> string_of_bool x

let rec rep_expr expr = match expr.data with
    | Lit(e1)           -> rep_literal e1
    | BinOp(e1, op, e2) -> rep_expr e1 ^ " " ^ op ^ " " ^ rep_expr e2
    | Neg(e1)           -> "-" ^ rep_expr e1
    | Call(name, exprs) -> name ^ "(" ^ String.concat ", " (List.map rep_expr exprs) ^ ")"
    | Paren(e1)         -> "(" ^ rep_expr e1 ^ ")"
    | Var(v)            -> v
    | Assign(ass)       -> rep_assign ass

and rep_assign ((name, typ), value, body) = "let " ^ name ^ ": " ^ rep_type typ ^ 
    " = " ^ rep_expr value ^ " in " ^ rep_expr body


let rep_func func = match func.data with

    | Func(def, args, body)  -> 
        "def "  ^ get_name def ^ "(" ^
        (* Ugly hack *)
        begin
            if (snd args <> TEmpty) then
                (String.concat ", " (List.map rep_var (conv_args args)))
            else
                ""
        end
        ^
        "): "^ (Types.rep_type (get_type def)) ^
        " = " ^ (rep_expr body) ^ ";"
    | Operator(def, args, body) ->
        let args = conv_args args in 
        "operator "  ^ get_name def ^ "(" ^
        (* Ugly hack *)
        (String.concat ", " (List.map rep_var args)) ^

        "): "^ (Types.rep_type (get_type def)) ^
        " = " ^ (rep_expr body) ^ ";"
    | Extern(def, args)     ->
        "extern " ^ get_name def ^ "(" ^
        begin
            if (snd args <> TEmpty) then
                String.concat ", " (List.map rep_var (conv_args args))
            else
                ""
        end
        ^ "): " ^ Types.rep_type (get_type def) ^ ";"

let print_ast = List.iter (fun x -> print_endline(rep_func x))