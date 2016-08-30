type tp = 
    | TInt 
    | TFloat 
    | TArrow of tp * tp
    | TEmpty

val rep_var     : (string * tp) -> string
val rep_type    : tp -> string
