(* Parser interface - exposes types and functions *)

(* Exceptions *)
exception ParseError of string

(* AST types *)
type cresta_type = 
  | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64
  | Float32 | Float64
  | Bool | String
  | Complex of cresta_type
  | Array of cresta_type * int list
  | UserDefined of string

type expression = 
  | IntLiteral of int
  | FloatLiteral of float  
  | BoolLiteral of bool
  | StringLiteral of string
  | Variable of string
  | BinaryOp of expression * string * expression
  | MemberAccess of expression * string
  | MethodCall of expression * string * expression list
  | ArrayAccess of expression * expression list

type visibility = Public | Private

type parameter = {
  param_type : cresta_type;
  param_name : string;
}

type statement = 
  | VarDecl of cresta_type * string * expression option
  | Assignment of string * expression
  | ExpressionStmt of expression
  | IfStmt of expression * statement list * statement list option
  | ForLoop of statement option * expression option * expression option * statement list
  | WhileLoop of expression * statement list
  | Block of statement list
  | FunctionDecl of function_decl
  | ClassDecl of class_decl
  | ReturnStmt of expression option

and function_decl = {
  visibility : visibility;
  return_type : cresta_type option;
  func_name : string;
  parameters : parameter list;
  body : statement list;
}

and class_member = 
| Field of visibility * cresta_type * string
| Method of function_decl

and class_decl = {
  class_name : string;
  members : class_member list;
}

type cresta_program = statement list

(* Parser state type *)
type parser_state = {
  tokens: Lexer.positioned_token list;
  position: int;
}

(* Parser functions *)
val parse_expression : parser_state -> expression * parser_state
val parse_equality_expression : parser_state -> expression * parser_state
val parse_comparison_expression : parser_state -> expression * parser_state
val parse_additive_expression : parser_state -> expression * parser_state
val parse_multiplicative_expression : parser_state -> expression * parser_state
val parse_member_expression : parser_state -> expression * parser_state
val parse_primary_expression : parser_state -> expression * parser_state
val parse_statement : parser_state -> statement * parser_state
val parse_variable_declaration : parser_state -> statement * parser_state
val parse_type : parser_state -> cresta_type * parser_state
val parse_argument_list : parser_state -> expression list * parser_state
val parse_statement_list : parser_state -> statement list * parser_state
val parse_visibility : parser_state -> visibility * parser_state
val parse_parameter_list : parser_state -> parameter list * parser_state
val parse_function_declaration : parser_state -> statement * parser_state
val parse_function_with_visibility : visibility -> parser_state -> function_decl * parser_state
val parse_class_member : parser_state -> class_member * parser_state
val parse_class_declaration : parser_state -> statement * parser_state
val parse_return_statement : parser_state -> statement * parser_state
val expect_token : Lexer.token -> parser_state -> parser_state
val current_token : parser_state -> Lexer.token option
val test_parse_primary : unit -> unit
val test_parse_multiplication : unit -> unit
val test_parse_complex_multiplication : unit -> unit
val test_parse_variable_declaration : unit -> unit
val test_parse_variable_declaration_no_init : unit -> unit
val test_parse_assignment : unit -> unit
val test_parse_complex_assignment : unit -> unit
val test_parse_member_access : unit -> unit
val test_parse_method_call : unit -> unit
val test_parse_method_call_with_args : unit -> unit
val test_parse_comparison : unit -> unit
val test_parse_chained_comparison : unit -> unit
val test_parse_addition : unit -> unit
val test_parse_scientific_addition : unit -> unit
val test_parse_complex_expression : unit -> unit
val test_parse_simple_if : unit -> unit
val test_parse_if_or_else : unit -> unit
val test_parse_if_or_elseif : unit -> unit
val test_parse_simple_function : unit -> unit
val test_parse_void_function : unit -> unit
val test_parse_simple_class : unit -> unit
val test_parse_return_statement : unit -> unit
