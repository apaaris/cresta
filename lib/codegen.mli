(* Code Generator Interface - converts AST to LLVM IR *)

open Parser

(* LLVM context and module types *)
type llvm_context = {
  context : Llvm.llcontext;
  the_module : Llvm.llmodule;
  builder : Llvm.llbuilder;
  mutable named_values : (string, Llvm.llvalue) Hashtbl.t;
  mutable class_types : (string, Llvm.lltype) Hashtbl.t;
}

(* Code generation errors *)
type codegen_error = {
  message : string;
  location : string;
}

exception CodegenError of codegen_error

(* Main code generation functions *)
val create_context : string -> llvm_context
val generate_program : llvm_context -> cresta_program -> Llvm.llmodule
val generate_statement : llvm_context -> statement -> Llvm.llvalue option
val generate_expression : llvm_context -> expression -> Llvm.llvalue
val generate_function : llvm_context -> function_decl -> Llvm.llvalue

(* Type conversion utilities *)
val cresta_type_to_llvm : llvm_context -> cresta_type -> Llvm.lltype

(* Utility functions *)
val print_module : Llvm.llmodule -> unit
val write_module_to_file : Llvm.llmodule -> string -> unit
val optimize_module : Llvm.llmodule -> unit

(* Error handling *)
val print_codegen_error : codegen_error -> unit
