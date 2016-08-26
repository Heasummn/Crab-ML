(* The env is the main interface that is used throughout our Semantic analysis to store type information *)

open Types

type type_env = Types.tp Table.t 
type var_env = Types.tp Table.t
(* Args + return value *)
type op_env = (Types.tp list * Types.tp) MultiTable.t

type ctx = {types: type_env; vars: var_env; ops: op_env}

let types = 
[
    (Symbol.symbol "int",Types. TInt);
    (Symbol.symbol "float", Types.TFloat)
]

let ops = 
[
    (Symbol.symbol "+", ([TInt; TInt], TInt));    
    (Symbol.symbol "+", ([TFloat; TFloat], TFloat));    
    (Symbol.symbol "-", ([TInt; TInt], TInt));    
    (Symbol.symbol "-", ([TFloat; TFloat], TFloat));    
    (Symbol.symbol "*", ([TInt; TInt], TInt));    
    (Symbol.symbol "*", ([TFloat; TFloat], TFloat));    
    (Symbol.symbol "/", ([TInt; TInt], TInt));    
    (Symbol.symbol "/", ([TFloat; TFloat], TFloat));    
]

let base_type_env = Table.of_list types

let base_var_env = Table.empty

let base_op_env = MultiTable.of_list ops

let lookup_type env name = Table.lookup name env

let base_ctx = {types = base_type_env; vars = base_var_env; ops = base_op_env}
