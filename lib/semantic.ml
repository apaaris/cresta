(* Cresta Semantic Analyzer - Type Checking and Symbol Table Management *)

open Parser

(* Utility function to convert type to string *)
let rec string_of_type = function
  | Int8 -> "int8"
  | UInt8 -> "uint8"
  | Int16 -> "int16"
  | UInt16 -> "uint16"
  | Int32 -> "int32"
  | UInt32 -> "uint32"
  | Int64 -> "int64"
  | UInt64 -> "uint64"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Bool -> "bool"
  | String -> "string"
  | Complex t -> "complex<" ^ string_of_type t ^ ">"
  | Array (t, dims) -> 
      let dim_str = String.concat "," (List.map string_of_int dims) in
      string_of_type t ^ "[" ^ dim_str ^ "]"
  | UserDefined name -> name

(* Semantic analysis errors *)
type semantic_error = {
  message : string;
  location : string;  (* For now, simple string description *)
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

(* Symbol table - maps identifier names to their symbol information *)
type symbol_table = {
  symbols : (string, symbol) Hashtbl.t;
  mutable current_scope : int;
  mutable scope_stack : int list;  (* Stack of scope levels for nested scopes *)
}

(* Semantic analysis context *)
type semantic_context = {
  symbol_table : symbol_table;
  mutable current_class : class_decl option;  (* Current class being analyzed *)
  mutable current_function : function_decl option;  (* Current function being analyzed *)
  errors : semantic_error list ref;
}

(* Create a new symbol table *)
let create_symbol_table () : symbol_table =
  {
    symbols = Hashtbl.create 100;
    current_scope = 0;
    scope_stack = [0];
  }

(* Create a new semantic context *)
let create_context () : semantic_context =
  {
    symbol_table = create_symbol_table ();
    current_class = None;
    current_function = None;
    errors = ref [];
  }

(* Enter a new scope *)
let enter_scope (ctx : semantic_context) : unit =
  let new_scope = ctx.symbol_table.current_scope + 1 in
  ctx.symbol_table.current_scope <- new_scope;
  ctx.symbol_table.scope_stack <- new_scope :: ctx.symbol_table.scope_stack

(* Exit current scope *)
let exit_scope (ctx : semantic_context) : unit =
  match ctx.symbol_table.scope_stack with
  | [] -> failwith "Cannot exit scope: no scopes to exit"
  | [_] -> failwith "Cannot exit global scope"
  | _ :: parent_scope :: rest ->
      (* Remove all symbols from current scope *)
      Hashtbl.filter_map_inplace (fun _name symbol ->
        if symbol.scope_level = ctx.symbol_table.current_scope then None else Some symbol
      ) ctx.symbol_table.symbols;
      ctx.symbol_table.current_scope <- parent_scope;
      ctx.symbol_table.scope_stack <- parent_scope :: rest

(* Add an error to the context *)
let add_error (ctx : semantic_context) (message : string) (location : string) : unit =
  let error = { message = message; location = location } in
  ctx.errors := error :: !(ctx.errors)

(* Look up a symbol in the symbol table *)
let lookup_symbol (ctx : semantic_context) (name : string) : symbol option =
  Hashtbl.find_opt ctx.symbol_table.symbols name

(* Add a symbol to the symbol table *)
let add_symbol (ctx : semantic_context) (name : string) (kind : symbol_kind) : bool =
  (* Check if symbol already exists in current scope *)
  match lookup_symbol ctx name with
  | Some existing_symbol when existing_symbol.scope_level = ctx.symbol_table.current_scope ->
      add_error ctx ("Symbol '" ^ name ^ "' is already declared in this scope") ("scope " ^ string_of_int ctx.symbol_table.current_scope);
      false
  | _ ->
      let symbol = {
        name = name;
        kind = kind;
        scope_level = ctx.symbol_table.current_scope;
      } in
      Hashtbl.replace ctx.symbol_table.symbols name symbol;
      true

(* Type checking functions *)

(* Check if two types are compatible *)
let rec types_compatible (t1 : cresta_type) (t2 : cresta_type) : bool =
  match (t1, t2) with
  | (Int8, Int8) | (UInt8, UInt8) | (Int16, Int16) | (UInt16, UInt16) 
  | (Int32, Int32) | (UInt32, UInt32) | (Int64, Int64) | (UInt64, UInt64)
  | (Float32, Float32) | (Float64, Float64) | (Bool, Bool) | (String, String) -> true
  | (Complex t1', Complex t2') -> types_compatible t1' t2'
  | (Array (t1', dims1), Array (t2', dims2)) -> 
      types_compatible t1' t2' && dims1 = dims2
  | (UserDefined name1, UserDefined name2) -> String.equal name1 name2
  | _ -> false

(* Get the type of an expression *)
let rec check_expression (ctx : semantic_context) (expr : expression) : cresta_type option =
  match expr with
  | IntLiteral _ -> Some Int32  (* Default integer type *)
  | FloatLiteral _ -> Some Float64  (* Default float type *)
  | BoolLiteral _ -> Some Bool
  | StringLiteral _ -> Some String
  | Variable name ->
      (match lookup_symbol ctx name with
       | Some { kind = Variable var_type; _ } -> Some var_type
       | Some { kind = Parameter param_type; _ } -> Some param_type
       | Some { kind = Function _; _ } -> 
           add_error ctx ("'" ^ name ^ "' is a function, not a variable") ("expression");
           None
       | Some { kind = Class _; _ } -> 
           add_error ctx ("'" ^ name ^ "' is a class, not a variable") ("expression");
           None
       | None -> 
           add_error ctx ("Undefined variable: " ^ name) ("expression");
           None)
  | BinaryOp (left, op, right) ->
      (match (check_expression ctx left, check_expression ctx right) with
       | (Some left_type, Some right_type) ->
           if types_compatible left_type right_type then
             (match op with
              | "+" | "-" | "*" | "/" | ".+" | ".-" | ".*" | "./" -> Some left_type
              | "<" | "<=" | ">" | ">=" | "==" | "!=" -> Some Bool
              | "=" -> 
                  (* Assignment expression - check that left side is a variable *)
                  (match left with
                   | Variable _ -> Some left_type  (* Assignment returns the assigned value type *)
                   | _ -> 
                       add_error ctx ("Left side of assignment must be a variable") ("expression");
                       None)
              | _ -> 
                  add_error ctx ("Unknown binary operator: " ^ op) ("expression");
                  None)
           else (
             add_error ctx ("Type mismatch in binary operation: cannot apply '" ^ op ^ "' to different types") ("expression");
             None
           )
       | _ -> None)
  | MemberAccess (obj_expr, member_name) ->
      (match check_expression ctx obj_expr with
       | Some (UserDefined class_name) ->
           (* Look up the class and check if member exists *)
           (match lookup_symbol ctx class_name with
            | Some { kind = Class class_decl; _ } ->
                (* Find the member in the class *)
                let rec find_member members =
                  match members with
                  | [] -> 
                      add_error ctx ("Class '" ^ class_name ^ "' has no member '" ^ member_name ^ "'") ("member access");
                      None
                  | Field (_, field_type, field_name) :: _ when field_name = member_name ->
                      Some field_type
                  | _ :: rest -> find_member rest
                in
                find_member class_decl.members
            | _ ->
                add_error ctx ("'" ^ class_name ^ "' is not a defined class") ("member access");
                None)
       | Some _ ->
           add_error ctx ("Cannot access member of non-class type") ("member access");
           None
       | None -> None)
  | MethodCall (obj_expr, method_name, args) ->
      (match check_expression ctx obj_expr with
       | Some (UserDefined class_name) ->
           (* Look up the class and check if method exists *)
           (match lookup_symbol ctx class_name with
            | Some { kind = Class class_decl; _ } ->
                (* Find the method in the class *)
                let rec find_method members =
                  match members with
                  | [] -> 
                      add_error ctx ("Class '" ^ class_name ^ "' has no method '" ^ method_name ^ "'") ("method call");
                      None
                  | Method func_decl :: _ when func_decl.func_name = method_name ->
                      (* Check argument types *)
                      if List.length args = List.length func_decl.parameters then
                        (* TODO: Check individual argument types *)
                        func_decl.return_type
                      else (
                        add_error ctx ("Method '" ^ method_name ^ "' expects " ^ string_of_int (List.length func_decl.parameters) ^ " arguments, got " ^ string_of_int (List.length args)) ("method call");
                        None
                      )
                  | _ :: rest -> find_method rest
                in
                find_method class_decl.members
            | _ ->
                add_error ctx ("'" ^ class_name ^ "' is not a defined class") ("method call");
                None)
       | Some _ ->
           add_error ctx ("Cannot call method on non-class type") ("method call");
           None
       | None -> None)
  | ArrayAccess (array_expr, index_exprs) ->
      (* Check the array expression type *)
      (match check_expression ctx array_expr with
       | Some (Array(elem_type, dimensions)) ->
           (* Check that we have the right number of indices *)
           let num_indices = List.length index_exprs in
           let num_dimensions = List.length dimensions in
           if num_indices <> num_dimensions then (
             add_error ctx (Printf.sprintf "Array access expects %d indices but got %d" num_dimensions num_indices) ("expression");
             None
           ) else (
             (* Check that all indices are integers *)
             let all_indices_valid = List.for_all (fun index_expr ->
               match check_expression ctx index_expr with
               | Some Int32 -> true  (* For now, only support int32 indices *)
               | Some other_type -> 
                   add_error ctx ("Array index must be int32, got: " ^ string_of_type other_type) ("expression");
                   false
               | None -> false
             ) index_exprs in
             if all_indices_valid then Some elem_type else None
           )
       | Some other_type ->
           add_error ctx ("Cannot index non-array type: " ^ string_of_type other_type) ("expression");
           None
       | None -> None)
  | ArrayLiteral elements ->
      (* Check that all elements have the same type *)
      (match elements with
       | [] -> 
           add_error ctx ("Empty array literals not supported") ("expression");
           None
       | first :: rest ->
           let first_type = check_expression ctx first in
           let all_same_type = List.for_all (fun elem ->
             let elem_type = check_expression ctx elem in
             match (first_type, elem_type) with
             | (Some t1, Some t2) -> types_compatible t1 t2
             | _ -> false
           ) rest in
           if all_same_type then
             match first_type with
             | Some elem_type -> Some (Array(elem_type, [List.length elements]))
             | None -> None
           else (
             add_error ctx ("Array literal elements must have the same type") ("expression");
             None
           ))

(* Check a statement *)
let rec check_statement (ctx : semantic_context) (stmt : statement) : unit =
  match stmt with
  | VarDecl (var_type, var_name, init_expr) ->
      (* Check if variable name is already declared in current scope *)
      if add_symbol ctx var_name (Variable var_type) then
        (* Check initializer expression if present *)
        (match init_expr with
         | Some expr ->
             (match check_expression ctx expr with
              | Some expr_type ->
                  if not (types_compatible var_type expr_type) then
                    add_error ctx ("Type mismatch: cannot assign expression of type to variable of different type") ("variable declaration")
              | None -> () (* Error already reported *))
         | None -> ())
      
  | Assignment (var_name, expr) ->
      (* Check if variable exists *)
      (match lookup_symbol ctx var_name with
       | Some { kind = Variable var_type; _ } | Some { kind = Parameter var_type; _ } ->
           (* Check if assigned expression type matches variable type *)
           (match check_expression ctx expr with
            | Some expr_type ->
                if not (types_compatible var_type expr_type) then
                  add_error ctx ("Type mismatch: cannot assign expression to variable of different type") ("assignment")
            | None -> () (* Error already reported *))
       | Some { kind = Function _; _ } ->
           add_error ctx ("Cannot assign to function '" ^ var_name ^ "'") ("assignment")
       | Some { kind = Class _; _ } ->
           add_error ctx ("Cannot assign to class '" ^ var_name ^ "'") ("assignment")
       | None ->
           add_error ctx ("Undefined variable: " ^ var_name) ("assignment"))
           
  | ExpressionStmt expr ->
      ignore (check_expression ctx expr)
      
  | IfStmt (condition, then_stmts, else_stmts) ->
      (* Check condition is boolean *)
      (match check_expression ctx condition with
       | Some Bool -> ()
       | Some _ -> 
           add_error ctx ("If condition must be boolean, got other type") ("if statement")
       | None -> () (* Error already reported *));
      (* Check then branch *)
      enter_scope ctx;
      List.iter (check_statement ctx) then_stmts;
      exit_scope ctx;
      (* Check else branch if present *)
      (match else_stmts with
       | Some stmts ->
           enter_scope ctx;
           List.iter (check_statement ctx) stmts;
           exit_scope ctx
       | None -> ())
       
  | Block stmts ->
      enter_scope ctx;
      List.iter (check_statement ctx) stmts;
      exit_scope ctx
      
  | FunctionDecl func_decl ->
      (* Add function to symbol table *)
      if add_symbol ctx func_decl.func_name (Function func_decl) then (
        (* Enter function scope *)
        enter_scope ctx;
        (* Add parameters to scope *)
        List.iter (fun param ->
          ignore (add_symbol ctx param.param_name (Parameter param.param_type))
        ) func_decl.parameters;
        (* Check function body *)
        let old_function = ctx.current_function in
        ctx.current_function <- Some func_decl;
        List.iter (check_statement ctx) func_decl.body;
        ctx.current_function <- old_function;
        (* Exit function scope *)
        exit_scope ctx
      )
      
  | ClassDecl class_decl ->
      (* Add class to symbol table *)
      if add_symbol ctx class_decl.class_name (Class class_decl) then (
        (* Enter class scope *)
        enter_scope ctx;
        let old_class = ctx.current_class in
        ctx.current_class <- Some class_decl;
        (* Check class members *)
        List.iter (fun member ->
          match member with
          | Field (_, field_type, field_name) ->
              ignore (add_symbol ctx field_name (Variable field_type))
          | Method method_decl ->
              check_statement ctx (FunctionDecl method_decl)
        ) class_decl.members;
        ctx.current_class <- old_class;
        (* Exit class scope *)
        exit_scope ctx
      )
      
  | ReturnStmt return_expr ->
      (match ctx.current_function with
       | Some func_decl ->
           (match (return_expr, func_decl.return_type) with
            | (None, None) -> () (* void function, void return *)
            | (Some expr, Some expected_type) ->
                (match check_expression ctx expr with
                 | Some actual_type ->
                     if not (types_compatible expected_type actual_type) then
                       add_error ctx ("Return type mismatch") ("return statement")
                 | None -> () (* Error already reported *))
            | (None, Some _) ->
                add_error ctx ("Function must return a value") ("return statement")
            | (Some _, None) ->
                add_error ctx ("Void function cannot return a value") ("return statement"))
       | None ->
           add_error ctx ("Return statement outside of function") ("return statement"))
           
  | ForLoop (init_stmt, condition, update_expr, body_stmts) ->
      (* Check init statement *)
      (match init_stmt with
       | Some stmt -> check_statement ctx stmt
       | None -> ());
      
      (* Check condition expression *)
      (match condition with
       | Some cond_expr -> 
           (match check_expression ctx cond_expr with
            | Some Bool -> ()  (* Good - boolean condition *)
            | Some other_type -> 
                add_error ctx ("For loop condition must be boolean, got: " ^ string_of_type other_type) ("statement")
            | None -> 
                add_error ctx ("Invalid expression in for loop condition") ("statement"))
       | None -> ());  (* No condition is valid (infinite loop) *)
      
      (* Check update expression *)
      (match update_expr with
       | Some update -> ignore (check_expression ctx update)
       | None -> ());
      
      (* Check body statements *)
      List.iter (check_statement ctx) body_stmts
      
  | WhileLoop (condition, body_stmts) ->
      (* Check condition expression *)
      (match check_expression ctx condition with
       | Some Bool -> ()  (* Good - boolean condition *)
       | Some other_type -> 
           add_error ctx ("While loop condition must be boolean, got: " ^ string_of_type other_type) ("statement")
       | None -> 
           add_error ctx ("Invalid expression in while loop condition") ("statement"));
      
      (* Check body statements *)
      List.iter (check_statement ctx) body_stmts

(* Main semantic analysis function *)
let analyze_program (program : cresta_program) : (semantic_context, semantic_error list) result =
  let ctx = create_context () in
  
  (* Check all top-level statements *)
  List.iter (check_statement ctx) program;
  
  (* Return result *)
  if List.length !(ctx.errors) = 0 then
    Ok ctx
  else
    Error (List.rev !(ctx.errors))

(* Helper function to print semantic errors *)
let print_semantic_error (error : semantic_error) : unit =
  Printf.printf "Semantic Error [%s]: %s\n" error.location error.message

let print_semantic_errors (errors : semantic_error list) : unit =
  List.iter print_semantic_error errors
