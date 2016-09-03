type type_env = Types.tp Table.t
type var_env = Types.tp Table.t
type func_env = (Types.tp * Types.tp) MultiTable.t


type ctx = {types: type_env; vars: var_env; funcs: func_env}


val ops             : (Symbol.t *  (Types.tp * Types.tp)) list
val base_type_env   : type_env
val base_var_env    : var_env
val base_op_env     : func_env
val lookup_type     : type_env -> Symbol.t -> Types.tp option
val base_ctx        : ctx
