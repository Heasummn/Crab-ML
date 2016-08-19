(* Each function is a scope, here we store the values of that function. 
	Functions can have more than one scope, but we will deal with that later *)

open Types

type t = tp Table.t

let base_var_env = Table.empty

let vars = ref base_var_env

let lookup tbl key exep = match Table.lookup (Symbol.symbol key) tbl with
	| Some x		-> x
	| None 			-> raise(exep)

let lookup_var name = lookup !vars name (Error.TypeError("Unknown variable " ^ name))

let add_var name ty = vars := Table.add (Symbol.symbol name) ty !vars