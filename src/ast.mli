type literal = 
	| Integer of int
	| Float of float

type ast = 
	| Add of ast * ast
	| Sub of ast * ast
	| Neg of ast
	| Paren of ast
	| Lit of literal