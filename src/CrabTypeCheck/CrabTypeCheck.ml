open CrabAst

exception TypeError of string

let type_of_lit = function
	| Integer _ 	-> Tint
	| Float _		-> Tfloat

let rec check_exn ty e = let ty' = type_of_expr e in 
	if ty' <> ty then
		raise(TypeError( rep_expr e ^
	" has type " ^ rep_type ty' ^ ", but is used as if it has type " ^
	rep_type ty))

and check ty e = let ty' = type_of_expr e in 
	ty' = ty

(* If you're reading this, God help you *)
and type_of_expr e = 
	let is_num e = 
		if check Tint e || check Tfloat e then true else raise(TypeError(
			rep_expr e ^ " has type " ^ rep_type (type_of_expr e) ^
			", but is used as if it has type int or float."))
	in
	let type_op e1 e2 = 
		(* We assume that e1 and e2 are both nums *)
		let bind e1 e2 = if check Tfloat e1 || check Tfloat e2 then Tfloat else Tint in
		if (is_num e1 && is_num e2) then bind e1 e2 else assert false
	in

	match e with 
	| Lit e1		-> type_of_lit e1
	| Paren e1		-> type_of_expr e1
	| Neg e1		-> if is_num e1 then type_of_expr e1 else assert false
	| Add (e1, e2)	-> type_op e1 e2
	| Mult (e1, e2) -> type_op e1 e2
	| Sub (e1, e2)	-> type_op e1 e2
	| Div (e1, e2)	-> type_op e1 e2

let type_func = function
	| Func(tp, _, expr)	-> check_exn tp expr

let typecheck tree = List.iter type_func tree