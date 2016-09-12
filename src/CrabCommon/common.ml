open Batteries

let unity x = x

let mangle_name name args =
	let mangled_args = List.map (fun tp -> (Types.rep_type tp).[0]) args in
	
	if name = "main" then
		name
	else
		(* Add the amount of args to avoid naming conflicts. *)
		name ^ String.of_list mangled_args ^ (string_of_int (List.length args)) 
	