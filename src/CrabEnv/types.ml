open Batteries

type tp = TInt | TFloat | TEmpty

let rep_type = function
	| TInt      -> "int"
	| TFloat    -> "float"
	| TEmpty	-> "empty"

let rep_var name = fst name ^ ": " ^ rep_type (snd name)
