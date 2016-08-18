type t

val base_var_env: t

val vars        : t ref

val lookup      : 'a Table.t -> string -> exn -> 'a

val lookup_var  : string -> Types.tp

val add_var     : string -> Types.tp -> unit 