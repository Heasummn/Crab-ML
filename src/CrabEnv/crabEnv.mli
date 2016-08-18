open Types
open Batteries

type type_env = tp Table.t
type var_env = Scope.t Stack.t

val base_type_env  	: type_env

val lookup_type 	: string -> tp

val types       	: type_env

val curr_scope		: Scope.t ref
val peek_scope		: Scope.t
val pop_scope		: Scope.t
val push_scope		: unit