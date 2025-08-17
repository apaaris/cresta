(* Manual test to show LLVM code generation with a real Cresta program *)
open Cresta_lib.Parser
open Cresta_lib.Codegen

let test_complete_program () =
  Printf.printf "=== Manual LLVM Code Generation Test ===\n";
  let ctx = create_context "complete_program" in
  
  (* Simulate: int32 factorial(int32 n) { ... } *)
  let factorial_func = {
    visibility = Public;
    return_type = Some Int32;
    func_name = "factorial";
    parameters = [
      { param_type = Int32; param_name = "n" }
    ];
    body = [
      VarDecl (Int32, "result", Some (IntLiteral 1));
      VarDecl (Int32, "i", Some (IntLiteral 1));
      WhileLoop (
        BinaryOp (Variable "i", "<=", Variable "n"),
        [
          Assignment ("result", BinaryOp (Variable "result", "*", Variable "i"));
          Assignment ("i", BinaryOp (Variable "i", "+", IntLiteral 1));
        ]
      );
      ReturnStmt (Some (Variable "result"))
    ]
  } in
  
  (* Generate the factorial function *)
  let stmt1 = FunctionDecl factorial_func in
  let _ = generate_statement ctx stmt1 in
  
  (* Create a main function that uses factorial *)
  let main_func = {
    visibility = Public;
    return_type = Some Int32;
    func_name = "main";
    parameters = [];
    body = [
      VarDecl (Int32, "x", Some (IntLiteral 5));
      VarDecl (Int32, "result", Some (MethodCall (Variable "factorial", "", [Variable "x"])));
      ReturnStmt (Some (Variable "result"))
    ]
  } in
  
  let stmt2 = FunctionDecl main_func in
  let _ = generate_statement ctx stmt2 in
  
  Printf.printf "\nGenerated Complete LLVM Program:\n";
  print_module ctx.the_module;
  
  (* Write to file *)
  write_module_to_file ctx.the_module "complete_program.ll";
  Printf.printf "\nLLVM IR written to: complete_program.ll\n"

let () = test_complete_program ()
