type tp = 
    | TInt 
    | TFloat
    | TBool 
    | TArrow of tp * tp
    | TEmpty

val rep_var     : (string * tp) -> string
val rep_type    : tp -> string
val arrow_list  : tp -> tp list
val list_arrow  : tp list -> tp
