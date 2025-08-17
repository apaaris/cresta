(* Simple test for the lexer *)
open Cresta_lib.Lexer

let test_tokens source =
  let tokens = tokenize source "test.cr" in
  List.iter (fun positioned_token ->
    let token_str = string_of_token positioned_token.token in
    Printf.printf "%s " token_str
  ) tokens;
  print_newline ()

let () = 
  Printf.printf "Testing: () {} []\n";
  test_tokens "() {} []";
  
  Printf.printf "Testing: <float64> x = 42;\n";
  test_tokens "<float64> x = 42;";
  
  Printf.printf "Testing: + - * /\n";
  test_tokens "+ - * /";
  
  Printf.printf "Testing: class if or while\n";
  test_tokens "class if or while";
  
  Printf.printf "Testing: true false myVariable\n";
  test_tokens "true false myVariable";
  
  Printf.printf "Testing: 42 123 0\n";
  test_tokens "42 123 0";
  
  Printf.printf "Testing: 3.14 0.5 42.0\n";
  test_tokens "3.14 0.5 42.0";
  
  Printf.printf "Testing: <float64> x = 42.5;\n";
  test_tokens "<float64> x = 42.5;";
  
  Printf.printf "Testing: \"hello world\"\n";
  test_tokens "\"hello world\"";
  
  Printf.printf "Testing: \"simple test\"\n";
  test_tokens "\"simple test\"";
  
  Printf.printf "Testing: <string> message = \"Hello, Cresta!\";\n";
  test_tokens "<string> message = \"Hello, Cresta!\";";
  
  Printf.printf "Testing: // line comment\n";
  test_tokens "// this is a comment";
  
  Printf.printf "Testing: x = 42; // comment after code\n";
  test_tokens "x = 42; // comment after code";
  
  Printf.printf "Testing: /* block comment */\n";
  test_tokens "/* this is a block comment */";
  
  Printf.printf "Testing: x /* comment */ = 42;\n";
  test_tokens "x /* comment */ = 42;";
  
  Printf.printf "Testing: /* multi\n   line */ comment\n";
  test_tokens "/* multi\n   line */ x = 42;";
  
  Printf.printf "Testing: division operator\n";
  test_tokens "x = y / z;";
  
  Printf.printf "Testing: -> arrow operator\n";
  test_tokens "obj->member";
  
  Printf.printf "Testing: :: scope operator\n";
  test_tokens "obj::method()";
  
  Printf.printf "Testing: << stream operator\n";
  test_tokens "output << \"hello\"";
  
  Printf.printf "Testing: <= >= operators\n";
  test_tokens "x <= y >= z";
  
  Printf.printf "Testing: == != operators\n";
  test_tokens "x == y != z";
  
  Printf.printf "Testing: element-wise operators\n";
  test_tokens "A .+ B .* C";
  
  Printf.printf "Testing: mixed operators\n";
  test_tokens "obj->value == 42 or obj::method() <= max"