(* AST types for Cresta *)

type cresta_type = 
| Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64
| Float32 | Float64
| Bool | String
| Complex of cresta_type  (* for c_int32, c_float64, etc. *)
| Array of cresta_type * int list  (* type[dim1, dim2, ...] *)
| UserDefined of string  (* for class names like Calculator *)

type expression = 
| IntLiteral of int
| FloatLiteral of float  
| BoolLiteral of bool
| StringLiteral of string
| Variable of string
| BinaryOp of expression * string * expression  (* left op right *)
| MemberAccess of expression * string           (* obj->member *)
| MethodCall of expression * string * expression list  (* obj::method(args) *)
| ArrayAccess of expression * expression list   (* array[i, j, k] *)
| ArrayLiteral of expression list               (* [1, 2, 3, 4, 5] *)

type visibility = Public | Private

type parameter = {
  param_type : cresta_type;
  param_name : string;
}

type statement = 
| VarDecl of cresta_type * string * expression option  (* <type> name = expr; *)
| Assignment of string * expression                     (* name = expr; *)
| ExpressionStmt of expression                          (* expr; *)
| IfStmt of expression * statement list * statement list option  (* if cond { stmts } or { stmts } *)
| ForLoop of statement option * expression option * expression option * statement list
| WhileLoop of expression * statement list
| Block of statement list                               (* { statements } *)
| FunctionDecl of function_decl                         (* function declaration *)
| ClassDecl of class_decl                               (* class declaration *)
| ReturnStmt of expression option                       (* return expr; or return; *)

and function_decl = {
  visibility : visibility;
  return_type : cresta_type option;  (* None for void *)
  func_name : string;
  parameters : parameter list;
  body : statement list;
}

and class_member = 
| Field of visibility * cresta_type * string  (* visibility <type> name; *)
| Method of function_decl                      (* method declaration *)

and class_decl = {
  class_name : string;
  members : class_member list;
}

type cresta_program = statement list

(* Parser state and helper functions *)
type parser_state = {
  tokens: Lexer.positioned_token list;
  position: int;
}

exception ParseError of string

(* Helper functions - you'll implement these *)
let current_token (state : parser_state) : Lexer.token option =
  (* TODO: Get current token from state.tokens[state.position] *)
  if state.position < List.length state.tokens then
    let positioned_token = List.nth state.tokens state.position in
    Some positioned_token.token
  else
    None

let advance (state : parser_state) : parser_state =
  (* TODO: Return new state with position + 1 *)
  {state with position = state.position + 1}

let expect_token (expected : Lexer.token) (state : parser_state) : parser_state =
  match current_token state with
  | Some token when token = expected -> 
      advance state
  | Some actual -> 
      raise (ParseError ("Expected " ^ Lexer.string_of_token expected ^ 
                        " but got " ^ Lexer.string_of_token actual))
  | None -> 
      raise (ParseError ("Expected " ^ Lexer.string_of_token expected ^ 
                        " but reached end of input"))

(* Parse expressions with precedence climbing *)
let rec parse_expression (state : parser_state) : expression * parser_state =
  parse_equality_expression state

and parse_equality_expression (state : parser_state) : expression * parser_state =
  let (left, state') = parse_comparison_expression state in
  let rec parse_equality_rest left state =
    match current_token state with
    | Some (Lexer.EQUAL) -> 
        let state' = advance state in
        let (right, state'') = parse_comparison_expression state' in
        parse_equality_rest (BinaryOp(left, "==", right)) state''
    | Some (Lexer.NOT_EQUAL) -> 
        let state' = advance state in
        let (right, state'') = parse_comparison_expression state' in
        parse_equality_rest (BinaryOp(left, "!=", right)) state''
    | _ -> (left, state)
  in
  parse_equality_rest left state'

and parse_comparison_expression (state : parser_state) : expression * parser_state =
  let (left, state') = parse_additive_expression state in
  let rec parse_comparison_rest left state =
    match current_token state with
    | Some (Lexer.LESS) -> 
        let state' = advance state in
        let (right, state'') = parse_additive_expression state' in
        parse_comparison_rest (BinaryOp(left, "<", right)) state''
    | Some (Lexer.LESS_EQUAL) -> 
        let state' = advance state in
        let (right, state'') = parse_additive_expression state' in
        parse_comparison_rest (BinaryOp(left, "<=", right)) state''
    | Some (Lexer.GREATER) -> 
        let state' = advance state in
        let (right, state'') = parse_additive_expression state' in
        parse_comparison_rest (BinaryOp(left, ">", right)) state''
    | Some (Lexer.GREATER_EQUAL) -> 
        let state' = advance state in
        let (right, state'') = parse_additive_expression state' in
        parse_comparison_rest (BinaryOp(left, ">=", right)) state''
    | _ -> (left, state)
  in
  parse_comparison_rest left state'

and parse_additive_expression (state : parser_state) : expression * parser_state =
  let (left, state') = parse_multiplicative_expression state in
  let rec parse_additive_rest left state =
    match current_token state with
    | Some (Lexer.PLUS) -> 
        let state' = advance state in
        let (right, state'') = parse_multiplicative_expression state' in
        parse_additive_rest (BinaryOp(left, "+", right)) state''
    | Some (Lexer.MINUS) -> 
        let state' = advance state in
        let (right, state'') = parse_multiplicative_expression state' in
        parse_additive_rest (BinaryOp(left, "-", right)) state''
    | Some (Lexer.DOT_PLUS) -> 
        let state' = advance state in
        let (right, state'') = parse_multiplicative_expression state' in
        parse_additive_rest (BinaryOp(left, ".+", right)) state''
    | Some (Lexer.DOT_MINUS) -> 
        let state' = advance state in
        let (right, state'') = parse_multiplicative_expression state' in
        parse_additive_rest (BinaryOp(left, ".-", right)) state''
    | _ -> (left, state)
  in
  parse_additive_rest left state'

and parse_multiplicative_expression (state : parser_state) : expression * parser_state =
  let (left, state') = parse_member_expression state in
  let rec parse_multiplicative_rest left state =
    match current_token state with
    | Some (Lexer.MULTIPLY) -> 
        let state' = advance state in
        let (right, state'') = parse_member_expression state' in
        parse_multiplicative_rest (BinaryOp(left, "*", right)) state''
    | Some (Lexer.DIVIDE) -> 
        let state' = advance state in
        let (right, state'') = parse_member_expression state' in
        parse_multiplicative_rest (BinaryOp(left, "/", right)) state''
    | Some (Lexer.DOT_MULTIPLY) -> 
        let state' = advance state in
        let (right, state'') = parse_member_expression state' in
        parse_multiplicative_rest (BinaryOp(left, ".*", right)) state''
    | Some (Lexer.DOT_DIVIDE) -> 
        let state' = advance state in
        let (right, state'') = parse_member_expression state' in
        parse_multiplicative_rest (BinaryOp(left, "./", right)) state''
    | _ -> (left, state)
  in
  parse_multiplicative_rest left state'

(* Parse argument list: a, b, c *)
and parse_argument_list (state : parser_state) : expression list * parser_state =
  match current_token state with
  | Some (Lexer.RPAREN) -> 
      (* Empty argument list *)
      ([], state)
  | _ -> 
      (* Parse first argument *)
      let (first_arg, state) = parse_expression state in
      let rec parse_argument_rest args state =
        match current_token state with
        | Some (Lexer.COMMA) -> 
            let state = advance state in  (* Skip comma *)
            let (arg, state) = parse_expression state in
            parse_argument_rest (arg :: args) state
        | _ -> (List.rev args, state)
      in
      parse_argument_rest [first_arg] state

and parse_member_expression (state : parser_state) : expression * parser_state =
  let (expr, state) = parse_primary_expression state in
  let rec parse_member_rest expr state =
    match current_token state with
    | Some (Lexer.ARROW) -> 
        (* Member access or method call: obj->member or obj->method() *)
        let state = advance state in  (* Skip -> *)
        let (member_name, state) = match current_token state with
          | Some (Lexer.IDENTIFIER name) -> (name, advance state)
          | Some token -> raise (ParseError ("Expected member name after ->, got: " ^ Lexer.string_of_token token))
          | None -> raise (ParseError "Expected member name after ->, got end of input")
        in
        (* Check if this is a method call (followed by parentheses) *)
        (match current_token state with
         | Some (Lexer.LPAREN) ->
             (* This is a method call: obj->method(args) *)
             let state = advance state in  (* Skip ( *)
             let (args, state) = parse_argument_list state in
             let state = expect_token Lexer.RPAREN state in
             parse_member_rest (MethodCall(expr, member_name, args)) state
         | _ ->
             (* This is member access: obj->member *)
             parse_member_rest (MemberAccess(expr, member_name)) state)
    | Some (Lexer.SCOPE) -> 
        (* Method call: obj::method() *)
        let state = advance state in  (* Skip :: *)
        let (method_name, state) = match current_token state with
          | Some (Lexer.IDENTIFIER name) -> (name, advance state)
          | Some token -> raise (ParseError ("Expected method name after ::, got: " ^ Lexer.string_of_token token))
          | None -> raise (ParseError "Expected method name after ::, got end of input")
        in
        (* Parse method arguments () *)
        let state = expect_token Lexer.LPAREN state in
        let (args, state) = parse_argument_list state in
        let state = expect_token Lexer.RPAREN state in
        parse_member_rest (MethodCall(expr, method_name, args)) state
    | Some (Lexer.LBRACKET) ->
        (* Array access: array[index] *)
        let state = advance state in  (* Skip [ *)
        let (index_expr, state) = parse_expression state in
        let state = expect_token Lexer.RBRACKET state in  (* Skip ] *)
        parse_member_rest (ArrayAccess(expr, [index_expr])) state
    | _ -> (expr, state)
  in
  parse_member_rest expr state

and parse_primary_expression (state : parser_state) : expression * parser_state =
  match current_token state with
  | Some (Lexer.INTEGER i) -> 
      (IntLiteral i, advance state)
  | Some (Lexer.FLOAT f) -> 
      (FloatLiteral f, advance state)
  | Some (Lexer.BOOL b) -> 
      (BoolLiteral b, advance state)
  | Some (Lexer.STRING s) -> 
      (StringLiteral s, advance state)
  | Some (Lexer.IDENTIFIER name) -> 
      (Variable name, advance state)
  | Some (Lexer.LBRACKET) ->
      (* Parse array literal: [expr1, expr2, expr3] *)
      let state = advance state in  (* Skip [ *)
      let rec parse_array_elements acc state =
        match current_token state with
        | Some (Lexer.RBRACKET) -> (List.rev acc, advance state)  (* End of array *)
        | _ when acc = [] ->
            (* First element *)
            let (expr, state) = parse_expression state in
            parse_array_elements [expr] state
        | _ ->
            (* Subsequent elements *)
            let state = expect_token Lexer.COMMA state in  (* Expect comma *)
            let (expr, state) = parse_expression state in
            parse_array_elements (expr :: acc) state
      in
      let (elements, state) = parse_array_elements [] state in
      (ArrayLiteral elements, state)
  | Some token -> 
      raise (ParseError ("Unexpected token in expression: " ^ Lexer.string_of_token token))
  | None -> 
      raise (ParseError "Unexpected end of input in expression")

let test_parse_primary () =
  let tokens = Lexer.tokenize "42" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_primary_expression state in
  match ast with
  | IntLiteral 42 -> Printf.printf "PASS: Successfully parsed 42!\n"
  | _ -> Printf.printf "FAIL: Failed to parse 42\n"

let test_parse_multiplication () =
  let tokens = Lexer.tokenize "2 * 3" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_multiplicative_expression state in
  match ast with
  | BinaryOp(IntLiteral 2, "*", IntLiteral 3) -> Printf.printf "PASS: Successfully parsed 2 * 3!\n"
  | _ -> Printf.printf "FAIL: Failed to parse 2 * 3\n"

let test_parse_complex_multiplication () =
  let tokens = Lexer.tokenize "a .* b / c" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_multiplicative_expression state in
  match ast with
  | BinaryOp(BinaryOp(Variable "a", ".*", Variable "b"), "/", Variable "c") -> 
      Printf.printf "PASS: Successfully parsed a .* b / c with correct associativity!\n"
  | _ -> Printf.printf "FAIL: Failed to parse a .* b / c correctly\n"

(* Statement parsing functions *)
let rec parse_statement (state : parser_state) : statement * parser_state =
  (* Skip any newlines first *)
  let rec skip_newlines state =
    match current_token state with
    | Some (Lexer.NEWLINE) -> skip_newlines (advance state)
    | _ -> state
  in
  let state = skip_newlines state in
  
  match current_token state with
  | Some (Lexer.LESS) -> 
      (* Could be variable declaration or function declaration: <type> name = expr; or <type> name() {} *)
      parse_variable_or_function_declaration state
  | Some (Lexer.VOID) ->
      (* Function declaration: void name() {} *)
      parse_function_declaration_without_visibility state
  | Some (Lexer.IDENTIFIER _) -> 
      (* Could be assignment or expression statement *)
      parse_assignment_or_expression state
  | Some (Lexer.IF) -> 
      (* If statement *)
      parse_if_statement state
  | Some (Lexer.FOR) ->
      (* For loop *)
      parse_for_statement state
  | Some (Lexer.WHILE) ->
      (* While loop *)
      parse_while_statement state
  | Some (Lexer.CLASS) -> 
      (* Class declaration *)
      parse_class_declaration state
  | Some (Lexer.PUBLIC) | Some (Lexer.PRIVATE) -> 
      (* Function declaration with explicit visibility (for classes) *)
      parse_function_declaration state
  | Some (Lexer.RETURN) -> 
      (* Return statement *)
      parse_return_statement state
  | Some token -> 
      raise (ParseError ("Unexpected token at start of statement: " ^ Lexer.string_of_token token))
  | None -> 
      raise (ParseError "Unexpected end of input in statement")

and parse_variable_or_function_declaration (state : parser_state) : statement * parser_state =
  (* Look ahead to see if this is a function or variable declaration *)
  (* <type> name = expr; (variable) or <type> name() {} (function) *)
  let state = expect_token Lexer.LESS state in  (* Skip < *)
  let (typ, state) = parse_type state in
  let state = expect_token Lexer.GREATER state in  (* Skip > *)
  
  (* Parse name *)
  let (name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected identifier, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected identifier, got end of input")
  in
  
  (* Check what comes next *)
  match current_token state with
  | Some (Lexer.LPAREN) ->
      (* This is a function declaration: <type> name() {} *)
      let state = advance state in  (* Skip ( *)
      let (parameters, state) = parse_parameter_list state in
      let state = expect_token Lexer.RPAREN state in
      let state = expect_token Lexer.LBRACE state in
      let (body, state) = parse_statement_list state in
      let state = expect_token Lexer.RBRACE state in
      
      let func_decl = {
        visibility = Public;  (* Default to public for global functions *)
        return_type = Some typ;
        func_name = name;
        parameters = parameters;
        body = body;
      } in
      (FunctionDecl(func_decl), state)
  | Some (Lexer.ASSIGN) | Some (Lexer.SEMICOLON) ->
      (* This is a variable declaration: <type> name = expr; or <type> name; *)
      parse_variable_declaration_with_type_and_name typ name state
  | Some token ->
      raise (ParseError ("Expected '(', '=', or ';' after variable/function name, got: " ^ Lexer.string_of_token token))
  | None ->
      raise (ParseError "Expected '(', '=', or ';' after variable/function name, got end of input")

and parse_function_declaration_without_visibility (state : parser_state) : statement * parser_state =
  (* Parse void function: void name() {} *)
  let state = expect_token Lexer.VOID state in  (* Skip void *)
  
  (* Parse function name *)
  let (func_name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected function name, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected function name, got end of input")
  in
  
  (* Parse parameter list () *)
  let state = expect_token Lexer.LPAREN state in
  let (parameters, state) = parse_parameter_list state in
  let state = expect_token Lexer.RPAREN state in
  
  (* Parse function body { } *)
  let state = expect_token Lexer.LBRACE state in
  let (body, state) = parse_statement_list state in
  let state = expect_token Lexer.RBRACE state in
  
  let func_decl = {
    visibility = Public;  (* Default to public for global functions *)
    return_type = None;   (* void *)
    func_name = func_name;
    parameters = parameters;
    body = body;
  } in
  (FunctionDecl(func_decl), state)

and parse_variable_declaration_with_type_and_name (typ : cresta_type) (name : string) (state : parser_state) : statement * parser_state =
  (* Continue parsing variable declaration after we already have type and name *)
  match current_token state with
  | Some (Lexer.ASSIGN) -> 
      let state = advance state in  (* Skip = *)
      let (expr, state) = parse_expression state in
      let state = expect_token Lexer.SEMICOLON state in
      (VarDecl(typ, name, Some expr), state)
  | Some (Lexer.SEMICOLON) ->
      let state = advance state in  (* Skip ; *)
      (VarDecl(typ, name, None), state)
  | Some token ->
      raise (ParseError ("Expected '=' or ';' in variable declaration, got: " ^ Lexer.string_of_token token))
  | None ->
      raise (ParseError "Expected '=' or ';' in variable declaration, got end of input")

and parse_variable_declaration (state : parser_state) : statement * parser_state =
  (* Parse <type> name = expr; *)
  let state = expect_token Lexer.LESS state in  (* < *)
  
  (* Parse the type *)
  let (cresta_type, state) = parse_type state in
  
  let state = expect_token Lexer.GREATER state in  (* > *)
  
  (* Parse variable name *)
  let (var_name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected identifier, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected identifier, got end of input")
  in
  
  (* Check for optional assignment *)
  let (init_expr, state) = match current_token state with
    | Some (Lexer.ASSIGN) -> 
        let state = advance state in  (* Skip = *)
        let (expr, state) = parse_expression state in
        (Some expr, state)
    | _ -> (None, state)
  in
  
  let state = expect_token Lexer.SEMICOLON state in  (* ; *)
  
  (VarDecl(cresta_type, var_name, init_expr), state)

and parse_assignment_or_expression (state : parser_state) : statement * parser_state =
  (* Parse identifier first *)
  let (var_name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected identifier, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected identifier, got end of input")
  in
  
  (* Check what comes next *)
  match current_token state with
  | Some (Lexer.ASSIGN) -> 
      (* It's an assignment: x = expr; *)
      let state = advance state in  (* Skip = *)
      let (expr, state) = parse_expression state in
      let state = expect_token Lexer.SEMICOLON state in  (* ; *)
      (Assignment(var_name, expr), state)
  | _ -> 
      (* It's an expression statement - need to reparse as full expression *)
      (* Go back to start and parse the whole thing as expression *)
      let original_state = { state with position = state.position - 1 } in
      let (expr, state) = parse_expression original_state in
      let state = expect_token Lexer.SEMICOLON state in  (* ; *)
      (ExpressionStmt(expr), state)

and parse_statement_list (state : parser_state) : statement list * parser_state =
  let rec skip_newlines state =
    match current_token state with
    | Some (Lexer.NEWLINE) -> skip_newlines (advance state)
    | _ -> state
  in
  let rec parse_statements acc state =
    let state = skip_newlines state in  (* Skip newlines *)
    match current_token state with
    | Some (Lexer.RBRACE) -> 
        (* End of block *)
        (List.rev acc, state)
    | None -> 
        (* Unexpected end of input *)
        raise (ParseError "Unexpected end of input in statement block")
    | _ -> 
        (* Parse another statement *)
        let (stmt, state') = parse_statement state in
        parse_statements (stmt :: acc) state'
  in
  parse_statements [] state


and parse_type (state : parser_state) : cresta_type * parser_state =
  (* First parse the base type *)
  let (base_type, state) = match current_token state with
    | Some (Lexer.INT8) -> (Int8, advance state)
    | Some (Lexer.UINT8) -> (UInt8, advance state)
    | Some (Lexer.INT16) -> (Int16, advance state)
    | Some (Lexer.UINT16) -> (UInt16, advance state)
    | Some (Lexer.INT32) -> (Int32, advance state)
    | Some (Lexer.UINT32) -> (UInt32, advance state)
    | Some (Lexer.INT64) -> (Int64, advance state)
    | Some (Lexer.UINT64) -> (UInt64, advance state)
    | Some (Lexer.FLOAT32) -> (Float32, advance state)
    | Some (Lexer.FLOAT64) -> (Float64, advance state)
    | Some (Lexer.BOOL_TYPE) -> (Bool, advance state)
    | Some (Lexer.STRING_TYPE) -> (String, advance state)
    | Some (Lexer.IDENTIFIER typename) -> (UserDefined typename, advance state)  (* User-defined types *)
    | Some token -> 
        raise (ParseError ("Expected type name, got: " ^ Lexer.string_of_token token))
    | None -> 
        raise (ParseError "Expected type name, got end of input")
  in
  
  (* Check for array dimensions: [size] *)
  let rec parse_array_dimensions acc_type state =
    match current_token state with
    | Some (Lexer.LBRACKET) ->
        let state = advance state in  (* Skip [ *)
        (* Parse array size *)
        let (size, state) = match current_token state with
          | Some (Lexer.INTEGER size) -> (size, advance state)
          | Some token -> raise (ParseError ("Expected array size (integer), got: " ^ Lexer.string_of_token token))
          | None -> raise (ParseError "Expected array size, got end of input")
        in
        let state = expect_token Lexer.RBRACKET state in  (* Skip ] *)
        let array_type = Array(acc_type, [size]) in
        parse_array_dimensions array_type state  (* Check for more dimensions *)
    | _ -> (acc_type, state)  (* No more array dimensions *)
  in
  
  parse_array_dimensions base_type state

and parse_visibility (state : parser_state) : visibility * parser_state =
  match current_token state with
  | Some (Lexer.PUBLIC) -> (Public, advance state)
  | Some (Lexer.PRIVATE) -> (Private, advance state)
  | Some token -> raise (ParseError ("Expected visibility (public/private), got: " ^ Lexer.string_of_token token))
  | None -> raise (ParseError "Expected visibility, got end of input")

and parse_parameter_list (state : parser_state) : parameter list * parser_state =
  let rec parse_params acc state =
    match current_token state with
    | Some (Lexer.RPAREN) -> 
        (* End of parameter list *)
        (List.rev acc, state)
    | _ -> 
        (* Parse parameter: <type> name *)
        let state = expect_token Lexer.LESS state in
        let (param_type, state) = parse_type state in
        let state = expect_token Lexer.GREATER state in
        let (param_name, state) = match current_token state with
          | Some (Lexer.IDENTIFIER name) -> (name, advance state)
          | Some token -> raise (ParseError ("Expected parameter name, got: " ^ Lexer.string_of_token token))
          | None -> raise (ParseError "Expected parameter name, got end of input")
        in
        let param = { param_type = param_type; param_name = param_name } in
        match current_token state with
        | Some (Lexer.COMMA) -> 
            let state = advance state in  (* Skip comma *)
            parse_params (param :: acc) state
        | _ -> 
            parse_params (param :: acc) state
  in
  parse_params [] state

and parse_function_declaration (state : parser_state) : statement * parser_state =
  (* Parse visibility *)
  let (visibility, state) = parse_visibility state in
  let (func_decl, state) = parse_function_with_visibility visibility state in
  (FunctionDecl(func_decl), state)

and parse_function_with_visibility (visibility : visibility) (state : parser_state) : function_decl * parser_state =
  (* Parse return type: <type> or void *)
  let (return_type, state) = match current_token state with
    | Some (Lexer.VOID) -> (None, advance state)
    | Some (Lexer.LESS) -> 
        let state = advance state in  (* Skip < *)
        let (rtype, state) = parse_type state in
        let state = expect_token Lexer.GREATER state in  (* Skip > *)
        (Some rtype, state)
    | Some token -> raise (ParseError ("Expected return type or 'void', got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected return type or 'void', got end of input")
  in
  
  (* Parse function name *)
  let (func_name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected function name, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected function name, got end of input")
  in
  
  (* Parse parameter list () *)
  let state = expect_token Lexer.LPAREN state in
  let (parameters, state) = parse_parameter_list state in
  let state = expect_token Lexer.RPAREN state in
  
  (* Parse function body { } *)
  let state = expect_token Lexer.LBRACE state in
  let (body, state) = parse_statement_list state in
  let state = expect_token Lexer.RBRACE state in
  
  let func_decl = {
    visibility = visibility;
    return_type = return_type;
    func_name = func_name;
    parameters = parameters;
    body = body;
  } in
  (func_decl, state)

and parse_class_member (state : parser_state) : class_member * parser_state =
  (* Parse visibility *)
  let (visibility, state) = parse_visibility state in
  
  (* Look ahead to determine if it's a method or field *)
  (* Methods start with: visibility <type> name( or visibility void name( *)
  (* Fields start with: visibility <type> name; *)
  
  match current_token state with
  | Some (Lexer.VOID) ->
      (* It's a void method: visibility void name(...) *)
      let (func_decl, state) = parse_function_with_visibility visibility state in
      (Method(func_decl), state)
  | Some (Lexer.LESS) ->
      (* It could be a method or field, need to look further *)
      (* Save current state to backtrack if needed *)
      let saved_state = state in
      let state = advance state in  (* Skip < *)
      let (_, state) = parse_type state in  (* Parse type *)
      let state = expect_token Lexer.GREATER state in  (* Skip > *)
      (match current_token state with
       | Some (Lexer.IDENTIFIER _) ->
           let state = advance state in  (* Skip identifier *)
           (match current_token state with
            | Some (Lexer.LPAREN) ->
                (* It's a method: visibility <type> name(...) *)
                let (func_decl, state) = parse_function_with_visibility visibility saved_state in
                (Method(func_decl), state)
            | _ ->
                (* It's a field: visibility <type> name; *)
                let state = saved_state in  (* Reset to saved state *)
                let state = expect_token Lexer.LESS state in
                let (field_type, state) = parse_type state in
                let state = expect_token Lexer.GREATER state in
                let (field_name, state) = match current_token state with
                  | Some (Lexer.IDENTIFIER name) -> (name, advance state)
                  | Some token -> raise (ParseError ("Expected field name, got: " ^ Lexer.string_of_token token))
                  | None -> raise (ParseError "Expected field name, got end of input")
                in
                let state = expect_token Lexer.SEMICOLON state in
                (Field(visibility, field_type, field_name), state))
       | Some token -> raise (ParseError ("Expected identifier after type, got: " ^ Lexer.string_of_token token))
       | None -> raise (ParseError "Expected identifier after type, got end of input"))
  | Some token -> raise (ParseError ("Expected type or 'void' after visibility, got: " ^ Lexer.string_of_token token))
  | None -> raise (ParseError "Expected type or 'void' after visibility, got end of input")

and parse_class_declaration (state : parser_state) : statement * parser_state =
  (* Parse 'class' keyword *)
  let state = expect_token Lexer.CLASS state in
  
  (* Parse class name *)
  let (class_name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected class name, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected class name, got end of input")
  in
  
  (* Parse class body { } *)
  let state = expect_token Lexer.LBRACE state in
  let rec skip_newlines state =
    match current_token state with
    | Some (Lexer.NEWLINE) -> skip_newlines (advance state)
    | _ -> state
  in
  let rec parse_members acc current_visibility state =
    let state = skip_newlines state in  (* Skip newlines *)
    match current_token state with
    | Some (Lexer.RBRACE) -> 
        (* End of class *)
        (List.rev acc, state)
    | Some (Lexer.PUBLIC) -> 
        (* New public section: public: *)
        let state = advance state in  (* Skip 'public' *)
        let state = expect_token Lexer.COLON state in  (* Expect : *)
        parse_members acc Public state
    | Some (Lexer.PRIVATE) -> 
        (* New private section: private: *)
        let state = advance state in  (* Skip 'private' *)
        let state = expect_token Lexer.COLON state in  (* Expect : *)
        parse_members acc Private state
    | Some (Lexer.VOID) ->
        (* Parse void method with current visibility *)
        let (func_decl, state) = parse_function_with_visibility current_visibility state in
        let member = Method(func_decl) in
        parse_members (member :: acc) current_visibility state
    | Some (Lexer.LESS) ->
        (* Parse field or method with current visibility *)
        let (member, state) = parse_class_member_with_visibility current_visibility state in
        parse_members (member :: acc) current_visibility state
    | Some token -> 
        raise (ParseError ("Expected class member, visibility section, or '}', got: " ^ Lexer.string_of_token token))
    | None -> 
        raise (ParseError "Unexpected end of input in class declaration")
  in
  let (members, state) = parse_members [] Private state in  (* Default to private *)
  let state = expect_token Lexer.RBRACE state in
  
  let class_decl = {
    class_name = class_name;
    members = members;
  } in
  (ClassDecl(class_decl), state)

and parse_class_member_with_visibility (visibility : visibility) (state : parser_state) : class_member * parser_state =
  (* Parse field or method starting with <type> *)
  let state = expect_token Lexer.LESS state in
  let (typ, state) = parse_type state in
  let state = expect_token Lexer.GREATER state in
  let (name, state) = match current_token state with
    | Some (Lexer.IDENTIFIER name) -> (name, advance state)
    | Some token -> raise (ParseError ("Expected identifier after type, got: " ^ Lexer.string_of_token token))
    | None -> raise (ParseError "Expected identifier after type, got end of input")
  in
  
  (* Check if it's a method (has parentheses) or field (has semicolon) *)
  match current_token state with
  | Some (Lexer.LPAREN) ->
      (* It's a method: <type> name(...) *)
      let state = advance state in  (* Skip ( *)
      let (parameters, state) = parse_parameter_list state in
      let state = expect_token Lexer.RPAREN state in
      let state = expect_token Lexer.LBRACE state in
      let (body, state) = parse_statement_list state in
      let state = expect_token Lexer.RBRACE state in
      
      let func_decl = {
        visibility = visibility;
        return_type = Some typ;
        func_name = name;
        parameters = parameters;
        body = body;
      } in
      (Method(func_decl), state)
  | Some (Lexer.SEMICOLON) ->
      (* It's a field: <type> name; *)
      let state = advance state in  (* Skip ; *)
      (Field(visibility, typ, name), state)
  | Some token ->
      raise (ParseError ("Expected '(' or ';' after field/method name, got: " ^ Lexer.string_of_token token))
  | None ->
      raise (ParseError "Expected '(' or ';' after field/method name, got end of input")

and parse_return_statement (state : parser_state) : statement * parser_state =
  (* Parse 'return' keyword *)
  let state = expect_token Lexer.RETURN state in
  
  (* Check for optional expression *)
  match current_token state with
  | Some (Lexer.SEMICOLON) -> 
      (* return; *)
      let state = advance state in
      (ReturnStmt(None), state)
  | _ -> 
      (* return expr; *)
      let (expr, state) = parse_expression state in
      let state = expect_token Lexer.SEMICOLON state in
      (ReturnStmt(Some expr), state)

and parse_for_statement (state : parser_state) : statement * parser_state =
  (* Parse for loop: for (init; condition; update) { body } *)
  let state = expect_token Lexer.FOR state in
  let state = expect_token Lexer.LPAREN state in
  
  (* Parse init statement (optional) *)
  let (init_stmt, state) = 
    match current_token state with
    | Some (Lexer.SEMICOLON) -> (None, advance state)  (* Empty init *)
    | Some (Lexer.LESS) ->
        (* Variable declaration: <type> name = expr; *)
        let state = expect_token Lexer.LESS state in
        let (typ, state) = parse_type state in
        let state = expect_token Lexer.GREATER state in
        let (name, state) = match current_token state with
          | Some (Lexer.IDENTIFIER name) -> (name, advance state)
          | _ -> failwith "Expected identifier after type in for loop"
        in
        let state = expect_token Lexer.ASSIGN state in
        let (expr, state) = parse_expression state in
        let state = expect_token Lexer.SEMICOLON state in
        (Some (VarDecl(typ, name, Some expr)), state)
    | Some (Lexer.IDENTIFIER _) ->
        (* Assignment: name = expr; *)
        let (stmt, state) = parse_assignment_or_expression state in
        let state = expect_token Lexer.SEMICOLON state in
        (Some stmt, state)
    | _ -> 
        failwith "Expected variable declaration or assignment in for loop init"
  in
  
  (* Parse condition (optional) *)
  let (condition, state) = 
    match current_token state with
    | Some (Lexer.SEMICOLON) -> (None, advance state)  (* Empty condition *)
    | _ -> 
        let (expr, state) = parse_expression state in
        let state = expect_token Lexer.SEMICOLON state in
        (Some expr, state)
  in
  
  (* Parse update expression (optional) *)
  let (update_expr, state) = 
    match current_token state with
    | Some (Lexer.RPAREN) -> (None, state)  (* Empty update *)
    | Some (Lexer.IDENTIFIER name) ->
        (* Check for assignment: name = expr *)
        let state = advance state in  (* Skip identifier *)
        (match current_token state with
         | Some (Lexer.ASSIGN) ->
             let state = advance state in  (* Skip = *)
             let (expr, state) = parse_expression state in
             (* Convert assignment to expression by wrapping *)
             (Some (BinaryOp(Variable name, "=", expr)), state)
         | _ ->
             failwith "Expected assignment in for loop update")
    | _ -> 
        let (expr, state) = parse_expression state in
        (Some expr, state)
  in
  
  let state = expect_token Lexer.RPAREN state in
  let state = expect_token Lexer.LBRACE state in
  let (body, state) = parse_statement_list state in
  let state = expect_token Lexer.RBRACE state in
  
  (ForLoop(init_stmt, condition, update_expr, body), state)

and parse_while_statement (state : parser_state) : statement * parser_state =
  (* Parse while loop: while (condition) { body } *)
  let state = expect_token Lexer.WHILE state in
  let state = expect_token Lexer.LPAREN state in
  let (condition, state) = parse_expression state in
  let state = expect_token Lexer.RPAREN state in
  let state = expect_token Lexer.LBRACE state in
  let (body, state) = parse_statement_list state in
  let state = expect_token Lexer.RBRACE state in
  
  (WhileLoop(condition, body), state)

and parse_if_statement (state : parser_state) : statement * parser_state =
  (* Parse if statement: if (condition) { then_body } [or { else_body }] *)
  let state = expect_token Lexer.IF state in
  let state = expect_token Lexer.LPAREN state in
  let (condition, state) = parse_expression state in
  let state = expect_token Lexer.RPAREN state in
  let state = expect_token Lexer.LBRACE state in
  let (then_body, state) = parse_statement_list state in
  let state = expect_token Lexer.RBRACE state in
  
  (* Check for optional else clause *)
  let (else_body, state) = 
    match current_token state with
    | Some (Lexer.OR) ->
        let state = advance state in  (* Skip 'or' *)
        let state = expect_token Lexer.LBRACE state in
        let (else_stmts, state) = parse_statement_list state in
        let state = expect_token Lexer.RBRACE state in
        (Some else_stmts, state)
    | _ -> (None, state)
  in
  
  (IfStmt(condition, then_body, else_body), state)

let test_parse_variable_declaration () =
  let tokens = Lexer.tokenize "<int32> x = 42;" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_variable_declaration state in
  match ast with
  | VarDecl(Int32, "x", Some(IntLiteral 42)) -> 
      Printf.printf "PASS: Successfully parsed <int32> x = 42;!\n"
  | _ -> Printf.printf "FAIL: Failed to parse variable declaration\n"

let test_parse_variable_declaration_no_init () =
  let tokens = Lexer.tokenize "<float64> y;" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_variable_declaration state in
  match ast with
  | VarDecl(Float64, "y", None) -> 
      Printf.printf "PASS: Successfully parsed <float64> y; (no initialization)!\n"
  | _ -> Printf.printf "FAIL: Failed to parse variable declaration without init\n"

let test_parse_assignment () =
  let tokens = Lexer.tokenize "x = 42;" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_assignment_or_expression state in
  match ast with
  | Assignment("x", IntLiteral 42) -> 
      Printf.printf "PASS: Successfully parsed x = 42;!\n"
  | _ -> Printf.printf "FAIL: Failed to parse assignment\n"

let test_parse_complex_assignment () =
  let tokens = Lexer.tokenize "result = a * b;" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_assignment_or_expression state in
  match ast with
  | Assignment("result", BinaryOp(Variable "a", "*", Variable "b")) -> 
      Printf.printf "PASS: Successfully parsed result = a * b;!\n"
  | _ -> Printf.printf "FAIL: Failed to parse complex assignment\n"

let test_parse_member_access () =
  let tokens = Lexer.tokenize "obj->field" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_member_expression state in
  match ast with
  | MemberAccess(Variable "obj", "field") -> 
      Printf.printf "PASS: Successfully parsed obj->field!\n"
  | _ -> Printf.printf "FAIL: Failed to parse member access\n"

let test_parse_method_call () =
  let tokens = Lexer.tokenize "obj::method()" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_member_expression state in
  match ast with
  | MethodCall(Variable "obj", "method", []) -> 
      Printf.printf "PASS: Successfully parsed obj::method()!\n"
  | _ -> Printf.printf "FAIL: Failed to parse method call\n"

let test_parse_method_call_with_args () =
  let tokens = Lexer.tokenize "obj::calculate(42, x)" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_member_expression state in
  match ast with
  | MethodCall(Variable "obj", "calculate", [IntLiteral 42; Variable "x"]) -> 
      Printf.printf "PASS: Successfully parsed obj::calculate(42, x)!\n"
  | _ -> Printf.printf "FAIL: Failed to parse method call with arguments\n"

let test_parse_comparison () =
  let tokens = Lexer.tokenize "x < 10" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_comparison_expression state in
  match ast with
  | BinaryOp(Variable "x", "<", IntLiteral 10) -> 
      Printf.printf "PASS: Successfully parsed x < 10!\n"
  | _ -> Printf.printf "FAIL: Failed to parse comparison\n"

let test_parse_chained_comparison () =
  let tokens = Lexer.tokenize "a <= b >= c" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_comparison_expression state in
  match ast with
  | BinaryOp(BinaryOp(Variable "a", "<=", Variable "b"), ">=", Variable "c") -> 
      Printf.printf "PASS: Successfully parsed a <= b >= c with correct associativity!\n"
  | _ -> Printf.printf "FAIL: Failed to parse chained comparison\n"

let test_parse_addition () =
  let tokens = Lexer.tokenize "a + b" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_additive_expression state in
  match ast with
  | BinaryOp(Variable "a", "+", Variable "b") -> 
      Printf.printf "PASS: Successfully parsed a + b!\n"
  | _ -> Printf.printf "FAIL: Failed to parse addition\n"

let test_parse_scientific_addition () =
  let tokens = Lexer.tokenize "A .+ B .- C" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_additive_expression state in
  match ast with
  | BinaryOp(BinaryOp(Variable "A", ".+", Variable "B"), ".-", Variable "C") -> 
      Printf.printf "PASS: Successfully parsed A .+ B .- C with correct associativity!\n"
  | _ -> Printf.printf "FAIL: Failed to parse scientific addition\n"

let test_parse_complex_expression () =
  let tokens = Lexer.tokenize "A + B * C .+ D" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_additive_expression state in
  match ast with
  | BinaryOp(BinaryOp(Variable "A", "+", BinaryOp(Variable "B", "*", Variable "C")), ".+", Variable "D") -> 
      Printf.printf "PASS: Successfully parsed A + B * C .+ D with correct precedence!\n"
  | _ -> Printf.printf "FAIL: Failed to parse complex expression\n"

let test_parse_simple_if () =
  let tokens = Lexer.tokenize "if x < 10 { y = 42; }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_if_statement state in
  match ast with
  | IfStmt(BinaryOp(Variable "x", "<", IntLiteral 10), 
           [Assignment("y", IntLiteral 42)], 
           None) -> 
      Printf.printf "PASS: Successfully parsed simple if statement!\n"
  | _ -> Printf.printf "FAIL: Failed to parse simple if statement\n"

let test_parse_if_or_else () =
  let tokens = Lexer.tokenize "if x < 10 { y = 1; } or { y = 2; }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_if_statement state in
  match ast with
  | IfStmt(BinaryOp(Variable "x", "<", IntLiteral 10), 
           [Assignment("y", IntLiteral 1)], 
           Some [Assignment("y", IntLiteral 2)]) -> 
      Printf.printf "PASS: Successfully parsed if/or else statement!\n"
  | _ -> Printf.printf "FAIL: Failed to parse if/or else statement\n"

let test_parse_if_or_elseif () =
  let tokens = Lexer.tokenize "if x < 10 { y = 1; } or x > 20 { y = 2; }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_if_statement state in
  match ast with
  | IfStmt(BinaryOp(Variable "x", "<", IntLiteral 10), 
           [Assignment("y", IntLiteral 1)], 
           Some [IfStmt(BinaryOp(Variable "x", ">", IntLiteral 20), 
                        [Assignment("y", IntLiteral 2)], None)]) -> 
      Printf.printf "PASS: Successfully parsed if/or elseif statement!\n"
  | _ -> Printf.printf "FAIL: Failed to parse if/or elseif statement\n"

let test_parse_simple_function () =
  let tokens = Lexer.tokenize "public <int32> add(<int32> a, <int32> b) { return a + b; }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_function_declaration state in
  match ast with
  | FunctionDecl({visibility=Public; return_type=Some(Int32); func_name="add"; parameters=_; body=_}) -> 
      Printf.printf "PASS: Successfully parsed simple function!\n"
  | _ -> Printf.printf "FAIL: Failed to parse simple function\n"

let test_parse_void_function () =
  let tokens = Lexer.tokenize "private void initialize() { x = 0; }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_function_declaration state in
  match ast with
  | FunctionDecl({visibility=Private; return_type=None; func_name="initialize"; parameters=[]; body=_}) -> 
      Printf.printf "PASS: Successfully parsed void function!\n"
  | _ -> Printf.printf "FAIL: Failed to parse void function\n"

let test_parse_simple_class () =
  let tokens = Lexer.tokenize "class Point { private <int32> x; public <int32> getX() { return x; } }" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_class_declaration state in
  match ast with
  | ClassDecl({class_name="Point"; members=_}) -> 
      Printf.printf "PASS: Successfully parsed simple class!\n"
  | _ -> Printf.printf "FAIL: Failed to parse simple class\n"

let test_parse_return_statement () =
  let tokens = Lexer.tokenize "return x + 1;" "test.cr" in
  let state = { tokens = tokens; position = 0 } in
  let (ast, _) = parse_return_statement state in
  match ast with
  | ReturnStmt(Some(_)) -> 
      Printf.printf "PASS: Successfully parsed return statement!\n"
  | _ -> Printf.printf "FAIL: Failed to parse return statement\n"