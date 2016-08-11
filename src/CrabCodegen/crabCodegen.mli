open CrabAst

val context         : Llvm.llcontext
val glob_module     : Llvm.llmodule
val builder         : Llvm.llbuilder

val dump_val        : Llvm.llvalue -> unit
val dump_mod        : Llvm.llmodule -> unit

val codegen_literal : literal -> Llvm.llvalue
val codegen_expr    : literal expr -> Llvm.llvalue
val codegen_func    : literal top -> Llvm.llvalue
val codegen_ast     : literal top list -> Llvm.llvalue list