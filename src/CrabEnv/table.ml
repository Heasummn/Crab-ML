open Batteries

include Map.Make(Symbol)

let lookup value env = if mem value env 
        then Some (find value env)
    else
        None

let of_list lst = of_enum (List.enum lst)
