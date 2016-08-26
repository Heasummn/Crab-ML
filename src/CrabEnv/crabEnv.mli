type type_env = Types.tp Table.t
type var_env = Types.tp Table.t
type op_env = (Types.tp list * Types.tp) MultiTable.t


type ctx = {types: type_env; vars: var_env; ops: op_env}

val base_type_env   : type_env
val base_var_env    : var_env
val base_op_env     : op_env
val lookup_type     : type_env -> Symbol.t -> Types.tp option
val base_ctx        : ctx
