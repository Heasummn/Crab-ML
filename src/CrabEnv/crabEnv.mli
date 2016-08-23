type type_env = Types.tp Table.t
type var_env = Types.tp Table.t

val base_type_env   : type_env
val lookup_type     : type_env -> Symbol.t -> Types.tp option
