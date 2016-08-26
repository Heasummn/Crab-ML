open Batteries
open Batteries.MultiMap

type 'a t = (Symbol.t, 'a) MultiMap.t

let lookup value env = if mem value env 
        then Some (find value env)
    else
        None

let of_list lst = of_enum (List.enum lst)
