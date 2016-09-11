open CrabEnv
open CrabParseTree
open Error

(* Ew globals. TODO: Remove this. *)
let glob_context = ref base_ctx

let conv_type ctx tp = 
	match Table.lookup (Symbol.symbol tp) ctx.types with
		| Some tp	-> tp
		| None		-> raise(TypeError("Unknown type " ^ tp))

let conv_lit lit = 
	let data = match lit.data with
		| Integer(x)		-> CrabAst.Integer(x)
		| Float(x)			-> CrabAst.Float(x)
		| Bool(x)			-> CrabAst.Bool(x)
	in
	{ CrabAst.data = data; CrabAst.position = lit.position; CrabAst.tp = Types.TEmpty }

let rec conv_expr expr = 
	let data = match expr.data with
		| Paren expr				-> CrabAst.Paren(conv_expr expr)
		| Neg expr					-> CrabAst.Neg(conv_expr expr)
		| BinOp(e1, op, e2) 		-> CrabAst.BinOp(conv_expr e1, op, conv_expr e2)
		| Lit l1					-> CrabAst.Lit(conv_lit l1)
		| Call(name, args)			-> CrabAst.Call(name, List.map conv_expr args)
		| Var x						-> CrabAst.Var(x)
		| Assign(tp, value, body)	-> let (name, tp) = tp in let tp = conv_type !glob_context tp in
			CrabAst.Assign((name, tp), conv_expr value, conv_expr body)
	in
	{ CrabAst.data = data; CrabAst.position = expr.position; CrabAst.tp = Types.TEmpty; }

let conv_toplevel expr = 
	let conv_type = conv_type !glob_context in
	let data = match expr.data with
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
	in 
	(data, expr.position, Types.TEmpty)
	
let convAst ctx tree = glob_context := ctx;

	(* Rmove anything that is None *)
	let data = List.filter
	(fun (data, _, _) -> match data with
		| Some _	-> true
		| None		-> false) 
	(List.map conv_toplevel tree) 
	in
	(* Convert all the options to a value *)
	let data = List.map (fun (data, loc, tp) -> match data with 
		| Some x 	-> (x, loc, tp)
		| None 		-> assert false) data

	in
	List.map (fun (data, position, tp) -> 
		{CrabAst.data = data; CrabAst.position = position; CrabAst.tp = tp}
	) data