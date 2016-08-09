open Llvm
open Ast

val context 		: Llvm.llcontext
val glob_module 	: Llvm.llmodule
val builder			: Llvm.llbuilder

val dump_val 		: Llvm.llvalue -> unit
val dump_mod		: Llvm.llmodule -> unit

val codegen_literal : Ast.literal -> Llvm.llvalue
val codegen_expr	: Ast.ast -> Llvm.llvalue
val codegen_ast		: Ast.ast list -> Llvm.llvalue list  