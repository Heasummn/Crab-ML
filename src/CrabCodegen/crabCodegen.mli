open CrabAst

val context         : Llvm.llcontext
val glob_module     : Llvm.llmodule
val builder         : Llvm.llbuilder

val dump_val        : Llvm.llvalue -> unit
val dump_mod        : Llvm.llmodule -> unit

val codegen_literal : literal -> Llvm.llvalue
val codegen_expr    : CrabEnv.ctx -> expr -> Llvm.llvalue
val codegen_func    : CrabEnv.ctx -> toplevel -> Llvm.llvalue
val codegen_ast     : CrabEnv.ctx -> toplevel list -> Llvm.llvalue list
