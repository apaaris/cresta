(* Semantic Analyzer Interface *)

open Parser

(* Semantic analysis errors *)
type semantic_error = {
  message : string;
  location : string;
}

exception SemanticError of semantic_error

(* Symbol information *)
type symbol_kind = 
  | Variable of cresta_type
  | Function of function_decl
  | Class of class_decl
  | Parameter of cresta_type

type symbol = {
  name : string;
  kind : symbol_kind;
  scope_level : int;
}

type symbol_table = {
  symbols : (string, symbol) Hashtbl.t;
  mutable current_scope : int;
  mutable scope_stack : int list;
}

type semantic_context = {
  symbol_table : symbol_table;
  mutable current_class : class_decl option;
  mutable current_function : function_decl option;
  errors : semantic_error list ref;
}

(* Main semantic analysis function *)
val analyze_program : cresta_program -> (semantic_context, semantic_error list) result

(* Helper functions *)
val print_semantic_error : semantic_error -> unit
val print_semantic_errors : semantic_error list -> unit

(* Utility functions for testing *)
val create_context : unit -> semantic_context
val check_expression : semantic_context -> expression -> cresta_type option
val check_statement : semantic_context -> statement -> unit
