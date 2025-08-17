(* Tests for Semantic Analyzer *)
open Cresta_lib.Parser
open Cresta_lib.Semantic

(* Helper function to parse source code into program *)
let parse_source (source : string) : cresta_program =
  let tokens = Cresta_lib.Lexer.tokenize source "test.cr" in
  let parser_state = { tokens = tokens; position = 0 } in
  let rec skip_newlines state =
    match current_token state with
    | Some (Cresta_lib.Lexer.NEWLINE) -> skip_newlines { state with position = state.position + 1 }
    | _ -> state
  in
  let rec parse_statements acc state =
    let state = skip_newlines state in
    match current_token state with
    | None -> (List.rev acc, state)
    | Some (Cresta_lib.Lexer.EOF) -> (List.rev acc, state)
    | Some _ -> 
        let (stmt, state') = parse_statement state in
        parse_statements (stmt :: acc) state'
  in
  let (program, _) = parse_statements [] parser_state in
  program

let test_valid_program () =
  let source = "<int32> x = 42;\n<float64> y = 3.14;\n" in
  let program = parse_source source in
  match analyze_program program with
  | Ok _ -> Printf.printf "PASS: Valid program analysis succeeded\n"
  | Error errors -> 
      Printf.printf "FAIL: Valid program analysis failed with %d errors\n" (List.length errors);
      print_semantic_errors errors

let test_type_mismatch () =
  let source = "<int32> x = \"hello\";\n" in
  let program = parse_source source in
  match analyze_program program with
  | Ok _ -> Printf.printf "FAIL: Type mismatch test should have failed!\n"
  | Error errors -> 
      if List.length errors = 1 then
        Printf.printf "PASS: Type mismatch correctly detected!\n"
      else
        Printf.printf "FAIL: Expected 1 error, got %d errors\n" (List.length errors)

let test_undefined_variable () =
  let source = "<int32> x = undefined_var;\n" in
  let program = parse_source source in
  match analyze_program program with
  | Ok _ -> Printf.printf "FAIL: Undefined variable test should have failed!\n"
  | Error errors -> 
      if List.length errors = 1 then
        Printf.printf "PASS: Undefined variable correctly detected!\n"
      else
        Printf.printf "FAIL: Expected 1 error, got %d errors\n" (List.length errors)

let test_class_member_access () =
  let source = "class Test { private <int32> value; }\n<Test> t;\n<int32> x = t->value;\n" in
  let program = parse_source source in
  match analyze_program program with
  | Ok _ -> Printf.printf "PASS: Class member access analysis succeeded\n"
  | Error errors -> 
      Printf.printf "FAIL: Class member access failed with %d errors\n" (List.length errors);
      print_semantic_errors errors

let test_function_declaration () =
  let source = "public <int32> add(<int32> a, <int32> b) { return a + b; }\n" in
  let program = parse_source source in
  match analyze_program program with
  | Ok _ -> Printf.printf "PASS: Function declaration analysis succeeded\n"
  | Error errors -> 
      Printf.printf "FAIL: Function declaration failed with %d errors\n" (List.length errors);
      print_semantic_errors errors

let () =
  Printf.printf "Testing Semantic Analyzer:\n\n";
  
  Printf.printf "Testing valid program:\n";
  test_valid_program ();
  
  Printf.printf "\nTesting type mismatch detection:\n";
  test_type_mismatch ();
  
  Printf.printf "\nTesting undefined variable detection:\n";
  test_undefined_variable ();
  
  Printf.printf "\nTesting class member access:\n";
  test_class_member_access ();
  
  Printf.printf "\nTesting function declaration:\n";
  test_function_declaration ();
  
  Printf.printf "\nSemantic analyzer tests completed!\n"
