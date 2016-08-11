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