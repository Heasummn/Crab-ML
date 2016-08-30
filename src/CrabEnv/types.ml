open Batteries

type tp = 
    | TInt
    | TFloat 
    | TArrow of tp * tp
    | TEmpty

let rec rep_type = function
	| TInt              -> "int"
	| TFloat            -> "float"
    | TArrow(tp1, tp2)  -> rep_type tp1 ^ " -> " ^ rep_type tp2
	| TEmpty	        -> "empty"

let rep_var name = fst name ^ ": " ^ rep_type (snd name)
