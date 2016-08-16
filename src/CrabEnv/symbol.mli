(*  Comparing actual strings is slow. 
    Therefore we use Symbols to compare data. *)
type t

val compare : t -> t -> int
val name    : t -> string
val symbol  : string -> t