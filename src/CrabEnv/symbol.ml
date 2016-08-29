open Batteries

type t = string * int

let next_sym = ref 0

let syms : (string, int) Hashtbl.t = Hashtbl.create 100

let compare (a: t) (b: t) = 
	compare (snd a) (snd b)

let name (symbol: t) = fst symbol

let symbol (str: string) = 
	match Hashtbl.find_option syms str with 
	| Some(id)	-> str, id
	| None ->
		(* Add a symbol with the next_sym value *)
		let sym = (str, !next_sym) in
			Hashtbl.add syms str !next_sym;
			incr next_sym;
            sym
