(* Simple test for LLVM code generation *)
open Cresta_lib.Parser
open Cresta_lib.Codegen

let test_simple_expression () =
  let ctx = create_context "test_module" in
  
  (* Test simple integer literal *)
  let expr = IntLiteral 42 in
  let result = generate_expression ctx expr in
  Printf.printf "Generated integer literal: %s\n" (Llvm.string_of_llvalue result);
  
  (* Test simple float literal *)
  let expr2 = FloatLiteral 3.14 in
  let result2 = generate_expression ctx expr2 in
  Printf.printf "Generated float literal: %s\n" (Llvm.string_of_llvalue result2);
  
  (* Test boolean literal *)
  let expr3 = BoolLiteral true in
  let result3 = generate_expression ctx expr3 in
  Printf.printf "Generated bool literal: %s\n" (Llvm.string_of_llvalue result3);
  
  (* Test simple addition *)
  let expr4 = BinaryOp (IntLiteral 5, "+", IntLiteral 3) in
  let result4 = generate_expression ctx expr4 in
  Printf.printf "Generated addition: %s\n" (Llvm.string_of_llvalue result4);
  
  (* Print the entire module *)
  Printf.printf "\nGenerated LLVM IR:\n";
  print_module ctx.the_module

let test_variable_declaration () =
  Printf.printf "Testing Variable Declarations\n";
  let ctx = create_context "var_test_module" in
  
  (* We need to create a function first because LLVM instructions must be inside functions *)
  let void_type = Llvm.void_type ctx.context in
  let func_type = Llvm.function_type void_type [||] in
  let func = Llvm.define_function "test_func" func_type ctx.the_module in
  let entry_block = Llvm.entry_block func in
  Llvm.position_at_end entry_block ctx.builder;
  
  (* Test: int x = 42; *)
  Printf.printf "Testing: int x = 42;\n";
  let stmt1 = VarDecl (Int32, "x", Some (IntLiteral 42)) in
  let _ = generate_statement ctx stmt1 in
  
  (* Test: x = 10; *)
  Printf.printf "Testing: x = 10;\n";
  let stmt2 = Assignment ("x", IntLiteral 10) in
  let _ = generate_statement ctx stmt2 in
  
  (* Test: use variable in expression *)
  Printf.printf "Testing: using variable x in expression\n";
  let expr = Variable "x" in
  let result = generate_expression ctx expr in
  Printf.printf "Variable x value: %s\n" (Llvm.string_of_llvalue result);
  
  (* Test: int y; (no initialization) *)
  Printf.printf "Testing: int y; (no initialization)\n";
  let stmt3 = VarDecl (Int32, "y", None) in
  let _ = generate_statement ctx stmt3 in
  
  (* Test: y = x + 5; *)
  Printf.printf "Testing: y = x + 5;\n";
  let stmt4 = Assignment ("y", BinaryOp (Variable "x", "+", IntLiteral 5)) in
  let _ = generate_statement ctx stmt4 in
  
  (* Add a return instruction to complete the function *)
  ignore (Llvm.build_ret_void ctx.builder);
  
  Printf.printf "\nModule with variables:\n";
  print_module ctx.the_module

let test_if_statement () =
  Printf.printf "\n=== Testing If Statements ===\n";
  let ctx = create_context "if_test_module" in
  
  (* Create a function to hold our if statement *)
  let void_type = Llvm.void_type ctx.context in
  let func_type = Llvm.function_type void_type [||] in
  let func = Llvm.define_function "test_if_func" func_type ctx.the_module in
  let entry_block = Llvm.entry_block func in
  Llvm.position_at_end entry_block ctx.builder;
  
  (* Test: int x = 5; *)
  Printf.printf "Setting up: int x = 5;\n";
  let stmt1 = VarDecl (Int32, "x", Some (IntLiteral 5)) in
  let _ = generate_statement ctx stmt1 in
  
  (* Test: int y = 0; *)
  Printf.printf "Setting up: int y = 0;\n";
  let stmt2 = VarDecl (Int32, "y", Some (IntLiteral 0)) in
  let _ = generate_statement ctx stmt2 in
  
  (* Test: if (x > 3) { y = 10; } else { y = 20; } *)
  Printf.printf "Testing: if (x > 3) { y = 10; } else { y = 20; }\n";
  let if_stmt = IfStmt (
    BinaryOp (Variable "x", ">", IntLiteral 3),  (* condition: x > 3 *)
    [Assignment ("y", IntLiteral 10)],           (* then: y = 10 *)
    Some [Assignment ("y", IntLiteral 20)]       (* else: y = 20 *)
  ) in
  let _ = generate_statement ctx if_stmt in
  
  (* Test: simple if without else *)
  Printf.printf "Testing: if (y == 10) { x = 100; }\n";
  let if_stmt2 = IfStmt (
    BinaryOp (Variable "y", "==", IntLiteral 10), (* condition: y == 10 *)
    [Assignment ("x", IntLiteral 100)],           (* then: x = 100 *)
    None                                          (* no else *)
  ) in
  let _ = generate_statement ctx if_stmt2 in
  
  (* Add return to complete the function *)
  ignore (Llvm.build_ret_void ctx.builder);
  
  Printf.printf "\nModule with if statements:\n";
  print_module ctx.the_module

let test_function_generation () =
  Printf.printf "\n=== Testing Function Generation ===\n";
  let ctx = create_context "function_test_module" in
  
  (* Test: int add(int a, int b) { return a + b; } *)
  Printf.printf "Testing: int add(int a, int b) { return a + b; }\n";
  let add_func = {
    visibility = Public;
    return_type = Some Int32;
    func_name = "add";
    parameters = [
      { param_type = Int32; param_name = "a" };
      { param_type = Int32; param_name = "b" }
    ];
    body = [
      ReturnStmt (Some (BinaryOp (Variable "a", "+", Variable "b")))
    ]
  } in
  let stmt1 = FunctionDecl add_func in
  let _ = generate_statement ctx stmt1 in
  
  (* Test: void print_number(int x) { } (empty function for now) *)
  Printf.printf "Testing: void print_number(int x) { }\n";
  let print_func = {
    visibility = Public;
    return_type = None;
    func_name = "print_number";
    parameters = [
      { param_type = Int32; param_name = "x" }
    ];
    body = []
  } in
  let stmt2 = FunctionDecl print_func in
  let _ = generate_statement ctx stmt2 in
  
  (* Test: main function that calls add *)
  Printf.printf "Testing: int main() { return add(5, 3); }\n";
  let main_func = {
    visibility = Public;
    return_type = Some Int32;
    func_name = "main";
    parameters = [];
    body = [
      ReturnStmt (Some (MethodCall (Variable "add", "", [IntLiteral 5; IntLiteral 3])))
    ]
  } in
  let stmt3 = FunctionDecl main_func in
  let _ = generate_statement ctx stmt3 in
  
  Printf.printf "\nModule with functions:\n";
  print_module ctx.the_module

let test_array_generation () =
  Printf.printf "\n=== Testing Array Generation ===\n";
  let ctx = create_context "array_test_module" in
  
  (* Create a function to hold our array operations *)
  let void_type = Llvm.void_type ctx.context in
  let func_type = Llvm.function_type void_type [||] in
  let func = Llvm.define_function "test_array_func" func_type ctx.the_module in
  let entry_block = Llvm.entry_block func in
  Llvm.position_at_end entry_block ctx.builder;
  
  (* Test: int[5] arr; (declare an array of 5 integers) *)
  Printf.printf "Testing: int[5] arr;\n";
  let stmt1 = VarDecl (Array (Int32, [5]), "arr", None) in
  let _ = generate_statement ctx stmt1 in
  
  (* Test: arr[0] (access first element) *)
  Printf.printf "Testing: accessing arr[0]\n";
  let array_access = ArrayAccess (Variable "arr", [IntLiteral 0]) in
  let access_result = generate_expression ctx array_access in
  Printf.printf "Array access result: %s\n" (Llvm.string_of_llvalue access_result);
  
  (* Test: arr[2] (access third element) *)
  Printf.printf "Testing: accessing arr[2]\n";
  let array_access2 = ArrayAccess (Variable "arr", [IntLiteral 2]) in
  let access_result2 = generate_expression ctx array_access2 in
  Printf.printf "Array access result: %s\n" (Llvm.string_of_llvalue access_result2);
  
  (* Add return to complete the function *)
  ignore (Llvm.build_ret_void ctx.builder);
  
  Printf.printf "\nModule with arrays:\n";
  print_module ctx.the_module

let test_loop_generation () =
  Printf.printf "\n=== Testing Loop Generation ===\n";
  let ctx = create_context "loop_test_module" in
  
  (* Create a function to hold our loop *)
  let void_type = Llvm.void_type ctx.context in
  let func_type = Llvm.function_type void_type [||] in
  let func = Llvm.define_function "test_loop_func" func_type ctx.the_module in
  let entry_block = Llvm.entry_block func in
  Llvm.position_at_end entry_block ctx.builder;
  
  (* Setup: int i = 0; *)
  Printf.printf "Testing: int i = 0;\n";
  let stmt1 = VarDecl (Int32, "i", Some (IntLiteral 0)) in
  let _ = generate_statement ctx stmt1 in
  
  (* Setup: int sum = 0; *)
  Printf.printf "Testing: int sum = 0;\n";
  let stmt2 = VarDecl (Int32, "sum", Some (IntLiteral 0)) in
  let _ = generate_statement ctx stmt2 in
  
  (* Test: while (i < 5) { sum = sum + i; i = i + 1; } *)
  Printf.printf "Testing: while (i < 5) { sum = sum + i; i = i + 1; }\n";
  let while_stmt = WhileLoop (
    BinaryOp (Variable "i", "<", IntLiteral 5),  (* condition: i < 5 *)
    [
      Assignment ("sum", BinaryOp (Variable "sum", "+", Variable "i"));  (* sum = sum + i *)
      Assignment ("i", BinaryOp (Variable "i", "+", IntLiteral 1));      (* i = i + 1 *)
    ]
  ) in
  let _ = generate_statement ctx while_stmt in
  
  (* Add return to complete the function *)
  ignore (Llvm.build_ret_void ctx.builder);
  
  Printf.printf "\nModule with loops:\n";
  print_module ctx.the_module

let test_class_generation () =
  Printf.printf "\n=== Testing Class Generation ===\n";
  let ctx = create_context "class_test_module" in
  
  (* Test: class Point { int x; int y; void set(int nx, int ny) { x = nx; y = ny; } } *)
  Printf.printf "Testing: class Point with fields and method\n";
  let point_class = {
    class_name = "Point";
    members = [
      Field (Public, Int32, "x");
      Field (Public, Int32, "y");
      Method {
        visibility = Public;
        return_type = None;
        func_name = "set";
        parameters = [
          { param_type = Int32; param_name = "nx" };
          { param_type = Int32; param_name = "ny" }
        ];
        body = [
          (* For now, just a simple empty body *)
        ]
      }
    ]
  } in
  let stmt1 = ClassDecl point_class in
  let _ = generate_statement ctx stmt1 in
  
  (* Test: Point p; (declare an object) *)
  Printf.printf "Testing: Point p;\n";
  let stmt2 = VarDecl (UserDefined "Point", "p", None) in
  let _ = generate_statement ctx stmt2 in
  
  Printf.printf "\nModule with classes:\n";
  print_module ctx.the_module

let () = 
  test_simple_expression ();
  test_variable_declaration ();
  test_if_statement ();
  test_function_generation ();
  test_array_generation ();
  test_loop_generation ();
  test_class_generation ()
