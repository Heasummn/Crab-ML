(* 	This pass makes sure that every function has a unique name.
	Runs as a stage 3 pass.
	TODO: Convert variable names as well *)

open CrabAst
open Types
open Batteries


let rec alphaConvExpr expr = 
	let data = match expr.data with
		| Lit _
		| Var _ 					-> Common.unity expr.data
		| Neg e1 					-> Neg(alphaConvExpr e1)
		| Paren e1					-> Paren(alphaConvExpr e1)

		| BinOp (e1, op, e2)		-> BinOp(alphaConvExpr e1, 
			Common.mangle_name op [e1.tp; e2.tp], alphaConvExpr e2)
		| Assign (x, value, body)	-> Assign(x, alphaConvExpr value, alphaConvExpr body)
		| Call(name, exprs) 			-> Call(
			Common.mangle_name name (List.map (fun x -> x.tp) exprs), 
			List.map alphaConvExpr exprs)
	in
	{ expr with data }



let alphaConvTop toplevel = 
	let data  = match toplevel.data with
		| Func((name, tp), args, body)		->
			let name = Common.mangle_name name (arrow_list (snd args)) in
			Func((name, tp), args, alphaConvExpr body)
		| Operator((name, tp), args, body)	->
			let name = Common.mangle_name name (arrow_list (snd args)) in
				Operator((name, tp), args, alphaConvExpr body)
		| Extern((name, tp), args)		->
			let name = Common.mangle_name name (arrow_list (snd args)) in
			Extern((name, tp), args)
		in
	{ toplevel with data }


let alphaConv expr = List.map alphaConvTop expr