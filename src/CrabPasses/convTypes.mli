(* This pass fills in type data and converts the Parse Tree to an AST. *)

val convTree : CrabEnv.ctx -> CrabParseTree.toplevel list -> CrabAst.toplevel list