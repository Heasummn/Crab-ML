type tp = TInt | TFloat | TEmpty
type var_tp = string * tp

let rep_type = function
	| TInt      -> "int"
	| TFloat    -> "float"
	| TEmpty	-> "empty"

let rep_var = function
	| name, typ -> name ^ ": " ^ rep_type typ

let get_name = fst
let get_type = snd 