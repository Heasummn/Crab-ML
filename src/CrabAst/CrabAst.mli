type literal = 
    | Integer of int
    | Float of float

type ast = 
    | Paren of ast
    | Neg of ast
    | Mult of ast * ast
    | Div of ast * ast
    | Add of ast * ast
    | Sub of ast * ast
    | Lit of literal