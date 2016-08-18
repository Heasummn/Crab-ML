open Batteries

include (Map.S with type key = Symbol.t)

val lookup	: key -> 'a t -> 'a option

val of_list : (key * 'a) list -> 'a t