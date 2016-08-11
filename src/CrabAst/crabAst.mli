type literal = 
    | Integer of int
    | Float of float

type 'a expr = 
    | Base of 'a
    | Paren of 'a expr
    | Neg of 'a expr
    | Mult of 'a expr * 'a expr
    | Div of 'a expr * 'a expr
    | Add of 'a expr * 'a expr
    | Sub of 'a expr * 'a expr
    | Lit of literal

type types = Tint | Tfloat

type 'b top = 
    (* def type name exprs *)
    | Func of types * string * 'b expr


(* This definition is magical.
 * What it allows us to do is pair any amount of information with our AST,
 * without too much fiddling. Such as type data, or w/e else we might want.
 *)
type 'a ast = Ast of 'a * ('a ast) top