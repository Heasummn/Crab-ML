(* The env is the main interface that is used throughout our Semantic analysis to store type information *)

type type_env = Types.tp Table.t 
type var_env = Types.tp Table.t

type ctx = {types: type_env; vars: var_env}

let types = 
[
    (Symbol.symbol "int",Types. TInt);
    (Symbol.symbol "float", Types.TFloat)
]

let base_type_env = Table.of_list types

let base_var_env = Table.empty

let lookup_type env name = Table.lookup name env

let base_ctx = {types = base_type_env; vars = base_var_env }
