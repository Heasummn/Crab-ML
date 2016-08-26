open Batteries

type 'a t = (Symbol.t, 'a) MultiMap.t


val lookup  : Symbol.t -> 'a t -> 'a BatSet.t option
val of_list : (Symbol.t * 'a) list -> 'a t
