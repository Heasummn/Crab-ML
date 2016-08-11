type literal = 
    | Integer of int
    | Float of float

type expr = 
	| Paren of expr
    | Neg of expr
    | Mult of expr * expr
    | Div of expr * expr
    | Add of expr * expr
    | Sub of expr * expr
    | Lit of literal

type types = Tint | Tfloat

type top = 
	(* def type name exprs *)
	| Func of types * string * expr 


(* This definition is magical.
 * What it allows us to do is pair any amount of information with our AST,
 * without too much fiddling. Such as type data, or w/e else we might want.
 *)
type 'a ast = Ast of 'a * ('a ast) * top