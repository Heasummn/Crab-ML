type tp = Tint | Tfloat | Tempty

let rep_type = function
	| Tint      -> "int"
	| Tfloat    -> "float"
	| Tempty	-> "empty"