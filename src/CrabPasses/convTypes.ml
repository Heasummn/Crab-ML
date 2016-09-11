open CrabEnv
open ParseTree
open Error

(* Ew globals. TODO: Remove this. *)
let glob_context = ref base_ctx

let conv_expr _ = assert false

let rec some_map values = match values with
	| Some x :: tl 	-> x :: some_map tl
	| None :: tl 	-> [] @ some_map tl
	| []			-> []

let conv_type ctx tp = 
	match Table.lookup (Symbol.symbol tp) ctx.types with
		| Some tp	-> tp
		| None		-> raise(TypeError("Unknown type " ^ tp))

let conv_toplevel (expr) :(CrabAst.simple_toplevel option) = 
	let conv_type = conv_type !glob_context in
	match expr.data with
		| Func((name, ret), (names, types), body) 		-> 
			let types = List.map conv_type types in
			let ret = conv_type ret in
			let body = conv_expr body in
			let types = Types.list_arrow types in
				Some (CrabAst.Func((name, ret), (names, types), body))
		| Operator((name, ret), (names, types), body) 	->
			let types = List.map conv_type types in
			let ret = conv_type ret in
			let body = conv_expr body in
			let types = Types.list_arrow types in
				Some (CrabAst.Operator((name, ret), (names, types), body))
		| Extern((name, ret), (names, types)) 			-> 
			let types = List.map conv_type types in
			let ret = conv_type ret in
			let types = Types.list_arrow types in
				Some (CrabAst.Extern((name, ret), (names, types)))
		| Typedef (name, value)							->
			let value = conv_type value in
			let types = Table.add (Symbol.symbol name) value !glob_context.types in
			let ctx = { !glob_context with types } in
			glob_context := ctx;
			None
	

let convAst ctx tree :(CrabAst.toplevel list) = glob_context := ctx;
	let data = some_map (List.map conv_toplevel tree) in
	List.map2 (fun expr data -> 
	{ CrabAst.data = data; CrabAst.position = expr.position; CrabAst.tp = Types.TEmpty })

	tree data