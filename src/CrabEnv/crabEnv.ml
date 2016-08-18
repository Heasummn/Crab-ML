open Types

type type_env = tp Table.t
type var_env = tp Table.t

let base_types = 
	[
	 ("int", TInt);
	 ("float", TFloat);
	]

let base_type_env = 
	(List.map (fun (name, ty) -> Symbol.symbol name, ty) base_types)
	|> Table.of_list

let base_var_env = 
	Table.empty

let vars = ref base_var_env
let types = base_type_env

let lookup tbl key exep = match Table.lookup (Symbol.symbol key) tbl with
	| Some x		-> x
	| None 			-> raise(exep)

let lookup_type name = lookup types name (Error.TypeError("Unknown type " ^ name))

let lookup_var name = lookup !vars name (Error.TypeError("Unknown variable" ^ name))

let add_var name ty = vars := Table.add (Symbol.symbol name) ty !vars