open Batteries

include Map.Make(Symbol)

let lookup key env = 
	if mem key env then 
		Some(find key env)
	else 
		None

let of_list lst =
    List.enum lst
	|> of_enum
