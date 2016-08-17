type tp = TInt | TFloat | TEmpty
type var_tp = string * tp

val rep_type 	: tp -> string
val rep_var		: var_tp -> string

val get_name	: var_tp -> string
val get_type	: var_tp -> tp