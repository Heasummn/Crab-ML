(* 	This module controls the stages of passes within the Compiler
	Stage 1 passes convert the Parse Tree to the AST incrementally 
	Stage 2 passes modify the AST.
	Stage 3 passes are run on the AST after it has been typed 
*)

let stage1 ctx tree = ConvTypes.convTree ctx tree
let stage2 _ ast = Common.unity ast
let stage3 _ ast = AlphaConv.alphaConv ast