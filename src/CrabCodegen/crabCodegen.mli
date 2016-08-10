open CrabAst

val context         : Llvm.llcontext
val glob_module     : Llvm.llmodule
val builder         : Llvm.llbuilder

val dump_val        : Llvm.llvalue -> unit
val dump_mod        : Llvm.llmodule -> unit

val codegen_literal : literal -> Llvm.llvalue
val codegen_expr    : expr -> Llvm.llvalue
val codegen_func 	: ast -> Llvm.llvalue list
val codegen_ast     : ast list -> Llvm.llvalue list list