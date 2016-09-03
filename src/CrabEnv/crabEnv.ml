(* The env is the main interface that is used throughout our Semantic analysis to store type information *)

open Types

type type_env = Types.tp Table.t 
type var_env = Types.tp Table.t
(* Args + return value *)
type func_env = (Types.tp * Types.tp) MultiTable.t

type ctx = {types: type_env; vars: var_env; funcs: func_env}

let types = 
[
    (Symbol.symbol "int",Types. TInt);
    (Symbol.symbol "float", Types.TFloat)
]

let ops = 
[
    (Symbol.symbol "+", (TArrow(TInt, TInt), TInt));    
    (Symbol.symbol "+", (TArrow(TFloat, TFloat), TFloat)); 
    (Symbol.symbol "-", (TArrow(TInt, TInt), TInt));    
    (Symbol.symbol "-", (TArrow(TFloat, TFloat), TFloat));
    (Symbol.symbol "*", (TArrow(TInt, TInt), TInt));    
    (Symbol.symbol "*", (TArrow(TFloat, TFloat), TFloat));    
    (Symbol.symbol "/", (TArrow(TInt, TInt), TInt));    
    (Symbol.symbol "/", (TArrow(TFloat, TFloat), TFloat));    
]

let base_type_env = Table.of_list types

let base_var_env = Table.empty

let base_op_env = MultiTable.of_list ops

let lookup_type env name = Table.lookup name env

let base_ctx = {types = base_type_env; vars = base_var_env; funcs = base_op_env}
