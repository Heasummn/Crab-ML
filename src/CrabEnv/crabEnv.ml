open Types

type type_env = Types.tp Table.t

let base_types = 
	[
	 ("int", TInt);
	 ("float", TFloat);
	]

let base_type_env = 
	(List.map (fun (name, ty) -> Symbol.symbol name, ty) base_types)
	|> Table.of_list