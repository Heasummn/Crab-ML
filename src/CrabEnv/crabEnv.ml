open Types

let base_types = 
	[
	 ("int", TInt);
	 ("float", TFloat);
	]

let type_env = 
	Table.of_list (List.map (fun (name, ty) -> Symbol.symbol name, ty) base_types)

let foo = Table.lookup