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

let rec arrow_list = function
    | TArrow(tp1, tp2)  -> arrow_list tp1 @ arrow_list tp2
    | x                 -> [x]


let list_arrow lst = 
    if List.length lst > 0 then
        List.fold_left (fun arrow arg ->
             TArrow(arrow, arg)) (List.hd lst) (List.tl lst)
    else
        TEmpty
