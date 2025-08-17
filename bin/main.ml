(* Cresta Compiler - Main Entry Point *)

(* Exceptions for argument parsing *)
exception Found_help
exception Parse_error of string

(* Type definitions for compiler options *)
type compile_options = {
  input_file : string;
  output_file : string option;
  verbose : bool;
  debug : bool;
}

(* Result type for argument parsing *)
type parse_result = 
  | Options of compile_options
  | Help
  | Error of string

let parse_args (args : string array) : parse_result =
  (* This function should:
     1. Check if args array has at least one argument (the input file)
     2. Look for optional flags like -v (verbose), -d (debug), -o (output file)
     3. Return Help if --help or -h is found
     4. Return Error with message if arguments are invalid
     5. Return Options with a properly filled compile_options record
     
     Example usage:
     - cresta input.cr           -> Options { input_file="input.cr"; output_file=None; verbose=false; debug=false }
     - cresta input.cr -o out.c  -> Options { input_file="input.cr"; output_file=Some "out.c"; verbose=false; debug=false }
     - cresta -v input.cr        -> Options { input_file="input.cr"; output_file=None; verbose=true; debug=false }
     - cresta --help             -> Help
     - cresta                    -> Error "No input file specified"
  *)
  let argc = Array.length args in
  if argc <= 1 then 
    Error "No input file specified"
  else
    (* Start with default options *)
    let options = ref {input_file = ""; output_file = None; verbose = false; debug = false} in
    let input_files = ref [] in
    let i = ref 1 in (* Skip program name *)
    
    try
      (* Process arguments one by one *)
      while !i < argc do
        match args.(!i) with
        | "-h" | "--help" -> 
            raise Found_help
        | "-v" -> 
            options := { !options with verbose = true };
            incr i
        | "-d" ->
            options := { !options with debug = true };
            incr i
        | "-o" -> 
            if !i + 1 >= argc then
              raise (Parse_error "Output file not specified after -o")
            else (
              options := { !options with output_file = Some args.(!i + 1) };
              i := !i + 2  (* Skip both -o and the filename *)
            )
        | arg when not (String.starts_with ~prefix:"-" arg) -> 
            input_files := arg :: !input_files;
            incr i
        | unknown -> 
            raise (Parse_error ("Unknown flag: " ^ unknown))
      done;
      
      (* Validate input files *)
      match List.rev !input_files with
      | [] -> Error "No input file specified"
      | [input_file] -> 
          options := { !options with input_file = input_file };
          Options !options
      | _ -> Error "Multiple input files specified, only one allowed"
      
    with
    | Found_help -> Help
    | Parse_error msg -> Error msg

let rec compile_file (options : compile_options) : unit =
  (* This is the main compilation pipeline *)
  Printf.printf "Compiling file: %s\n" options.input_file;
  
  if options.verbose then (
    Printf.printf "Verbose mode enabled\n";
    Printf.printf "Debug mode: %b\n" options.debug;
    match options.output_file with
    | Some out -> Printf.printf "Output file: %s\n" out
    | None -> Printf.printf "No output file specified\n"
  );
  
  try
    (* Phase 1: Read source file *)
    if options.verbose then Printf.printf "Phase 1: Reading source file...\n";
    let source_code = 
      let ic = open_in options.input_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    if options.debug then Printf.printf "Source code length: %d characters\n" (String.length source_code);
    
    (* Phase 2: Lexical Analysis *)
    if options.verbose then Printf.printf "Phase 2: Lexical analysis (tokenization)...\n";
    let tokens = Cresta_lib.Lexer.tokenize source_code options.input_file in
    if options.debug then Printf.printf "Generated %d tokens\n" (List.length tokens);
    
    (* Phase 3: Parsing *)
    if options.verbose then Printf.printf "Phase 3: Parsing (AST generation)...\n";
    let parser_state = { Cresta_lib.Parser.tokens = tokens; position = 0 } in
    let (program, _final_state) = parse_program parser_state in
    if options.debug then Printf.printf "Successfully parsed program with %d top-level statements\n" (List.length program);
    
    (* Phase 4: Semantic Analysis *)
    if options.verbose then Printf.printf "Phase 4: Semantic analysis...\n";
    (match Cresta_lib.Semantic.analyze_program program with
     | Ok _semantic_context ->
         if options.debug then Printf.printf "Semantic analysis completed successfully\n"
     | Error semantic_errors ->
         if options.verbose then Printf.printf "Semantic analysis failed with %d errors:\n" (List.length semantic_errors);
         Cresta_lib.Semantic.print_semantic_errors semantic_errors;
         failwith "Semantic analysis failed");
    
    (* Phase 5: LLVM Code Generation *)
    if options.verbose then Printf.printf "Phase 5: LLVM IR generation...\n";
    let output_filename = match options.output_file with
      | Some filename -> filename
      | None -> 
          (* Generate output filename from input filename - now defaults to .ll *)
          let base = Filename.remove_extension options.input_file in
          base ^ ".ll"
    in
    if options.debug then Printf.printf "Writing LLVM IR to: %s\n" output_filename;
    
    (* Generate LLVM IR using our code generator *)
    let llvm_ctx = Cresta_lib.Codegen.create_context (Filename.basename options.input_file) in
    let llvm_module = Cresta_lib.Codegen.generate_program llvm_ctx program in
    
    (* Write LLVM IR to file *)
    Cresta_lib.Codegen.write_module_to_file llvm_module output_filename;
    
    if options.verbose then Printf.printf "LLVM IR written to %s\n" output_filename;
    Printf.printf "Successfully compiled %s -> %s\n" options.input_file output_filename;
    
  with
  | Sys_error msg -> 
      failwith ("File error: " ^ msg)
  | Cresta_lib.Parser.ParseError(msg) -> 
      failwith ("Parse error: " ^ msg)
  | exn -> 
      failwith ("Unexpected error during compilation: " ^ Printexc.to_string exn)

(* Parse a complete program (list of statements) *)
and parse_program (state : Cresta_lib.Parser.parser_state) : Cresta_lib.Parser.cresta_program * Cresta_lib.Parser.parser_state =
  let rec skip_newlines state =
    match Cresta_lib.Parser.current_token state with
    | Some (Cresta_lib.Lexer.NEWLINE) -> skip_newlines { state with position = state.position + 1 }
    | _ -> state
  in
  let rec parse_statements acc state =
    let state = skip_newlines state in  (* Skip leading newlines *)
    match Cresta_lib.Parser.current_token state with
    | None -> (List.rev acc, state)  (* End of input *)
    | Some (Cresta_lib.Lexer.EOF) -> (List.rev acc, state)  (* EOF token *)
    | Some _ -> 
        let (stmt, state') = Cresta_lib.Parser.parse_statement state in
        parse_statements (stmt :: acc) state'
  in
  parse_statements [] state

(* Helper function to print usage information *)
let print_usage () =
  print_endline "Cresta Compiler - Scientific Computing Language";
  print_endline "";
  print_endline "Usage: cresta [options] <input_file>";
  print_endline "";
  print_endline "Options:";
  print_endline "  -o <file>    Specify output file (default: <input>.ll)";
  print_endline "  -v           Verbose mode (show compilation phases)";
  print_endline "  -d           Debug mode (preserve intermediate representations)";
  print_endline "  -h, --help   Show this help message";
  print_endline "";
  print_endline "Examples:";
  print_endline "  cresta program.cr                    # Compile to program.ll";
  print_endline "  cresta -v program.cr                 # Verbose compilation";
  print_endline "  cresta -o output.ll program.cr       # Custom output file";
  print_endline "  cresta -d program.cr                 # Debug with intermediate files";
  print_endline "";
  print_endline "Generated LLVM IR can be compiled to native code with:";
  print_endline "  llc program.ll -o program.s         # Generate assembly";
  print_endline "  clang program.s -o program           # Link to executable"

(* Main function - entry point *)
let main () =
  let args = Sys.argv in
  match parse_args args with
  | Help -> 
      print_usage ();
      exit 0
  | Error msg -> 
      Printf.eprintf "Error: %s\n" msg;
      print_usage ();
      exit 1
  | Options options ->
      try
        compile_file options;
        Printf.printf "Compilation completed successfully.\n"
      with
      | Failure msg -> 
          Printf.eprintf "Compilation failed: %s\n" msg;
          exit 1
      | exn -> 
          Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
          exit 1

(* Run the main function *)
let () = main ()
