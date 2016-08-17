open Types

type type_env = tp Table.t
type var_env = var_tp Table.t

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