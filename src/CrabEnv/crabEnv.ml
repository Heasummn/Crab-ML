open Types
open Batteries
open Scope

type type_env = tp Table.t
type var_env = Scope.t Stack.t

let env = Stack.create ()

let base_types = 
	[
	 ("int", TInt);
	 ("float", TFloat);
	]

let base_type_env = 
	(List.map (fun (name, ty) -> Symbol.symbol name, ty) base_types)
	|> Table.of_list

let types = base_type_env

let lookup_type name = lookup types
	name (Error.TypeError("Unknown type " ^ name))


let curr_scope = (Stack.push base_var_env env); ref (Stack.top env)

let peek_scope = Stack.top env
let pop_scope = curr_scope := Stack.pop env; !curr_scope 
let push_scope = Stack.push base_var_env env; curr_scope := peek_scope 