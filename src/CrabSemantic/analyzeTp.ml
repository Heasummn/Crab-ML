open CrabAst
open Types
open Error

let exists_exn e1 err_message = match e1 with
			| Some x    -> x
			| None      -> raise(
				TypeError(err_message))

let def_exists_exn e1 = exists_exn e1 "This should never happen yet, but fill it in!"

let analyze_lit lit =
	let tp' = match lit.data with
		| Integer _     -> Some Tint
		| Float _       -> Some Tfloat
	in
	{ lit with tp = tp' }
