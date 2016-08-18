type tp = TInt | TFloat | TEmpty

type type_env = tp Table.t
type var_env = tp Table.t

val base_type_env   : type_env
val base_var_env    : var_env

val rep_type    : tp -> string
val vars        : var_env ref
val types       : type_env

val lookup_type : string -> tp
val lookup_var  : string -> tp

val rep_var     : (string * tp) -> string

val add_var     : string -> tp -> unit