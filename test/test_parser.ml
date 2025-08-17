(* Simple test for the parser *)
open Cresta_lib.Parser

let () = 
  Printf.printf "Testing primary expression parser:\n";
  test_parse_primary ();
  
  Printf.printf "Testing multiplication expression parser:\n";
  test_parse_multiplication ();
  
  Printf.printf "Testing complex multiplication (associativity):\n";
  test_parse_complex_multiplication ();
  
  Printf.printf "Testing variable declaration:\n";
  test_parse_variable_declaration ();
  
  Printf.printf "Testing variable declaration (no init):\n";
  test_parse_variable_declaration_no_init ();
  
  Printf.printf "Testing assignment statement:\n";
  test_parse_assignment ();
  
  Printf.printf "Testing complex assignment:\n";
  test_parse_complex_assignment ();
  
  Printf.printf "Testing member access:\n";
  test_parse_member_access ();
  
  Printf.printf "Testing method call (no args):\n";
  test_parse_method_call ();
  
  Printf.printf "Testing method call (with args):\n";
  test_parse_method_call_with_args ();
  
  Printf.printf "Testing comparison operators:\n";
  test_parse_comparison ();
  
  Printf.printf "Testing chained comparisons:\n";
  test_parse_chained_comparison ();
  
  Printf.printf "Testing addition:\n";
  test_parse_addition ();
  
  Printf.printf "Testing scientific addition:\n";
  test_parse_scientific_addition ();
  
  Printf.printf "Testing complex expression (precedence):\n";
  test_parse_complex_expression ();
  
  Printf.printf "Testing simple if statement:\n";
  test_parse_simple_if ();
  
  Printf.printf "Testing if/or else statement:\n";
  test_parse_if_or_else ();
  
  Printf.printf "Testing if/or elseif statement:\n";
  test_parse_if_or_elseif ();
  
  Printf.printf "Testing simple function:\n";
  test_parse_simple_function ();
  
  Printf.printf "Testing void function:\n";
  test_parse_void_function ();
  
  Printf.printf "Testing simple class:\n";
  test_parse_simple_class ();
  
  Printf.printf "Testing return statement:\n";
  test_parse_return_statement ()
