(* Cresta Lexer - Converts source text into tokens *)

(* Position information for error reporting *)
type position = {
  line : int;
  column : int;
  filename : string;
}

(* Token types for the Cresta language *)
type token = 
  (* Literals *)
  | INTEGER of int
  | FLOAT of float
  | STRING of string
  | BOOL of bool
  
  (* Identifiers and Keywords *)
  | IDENTIFIER of string
  | CLASS
  | PUBLIC | PRIVATE
  | IF | OR    (* "or" instead of "else if" *)
  | FOR | WHILE
  | RETURN
  | VOID
  
  (* Type keywords *)
  | INT8 | UINT8 | INT16 | UINT16 | INT32 | UINT32 | INT64 | UINT64
  | FLOAT32 | FLOAT64
  | STRING_TYPE
  | BOOL_TYPE
  
  (* Complex type prefix *)
  | C_PREFIX    (* for c_int8, c_float64, etc. *)
  
  (* Operators *)
  | ASSIGN      (* = *)
  | PLUS        (* + *)
  | MINUS       (* - *)
  | MULTIPLY    (* * *)
  | DIVIDE      (* / *)
  | DOT_PLUS    (* .+ *)
  | DOT_MINUS   (* .- *)
  | DOT_MULTIPLY (* .* *)
  | DOT_DIVIDE  (* ./ *)
  
  (* Comparison *)
  | EQUAL       (* == *)
  | NOT_EQUAL   (* != *)
  | LESS        (* < *)
  | LESS_EQUAL  (* <= *)
  | GREATER     (* > *)
  | GREATER_EQUAL (* >= *)
  
  (* Special operators *)
  | ARROW       (* -> for member access *)
  | SCOPE       (* :: for method access *)
  | STREAM      (* << for string streaming *)
  
  (* Punctuation *)
  | LANGLE      (* < *)
  | RANGLE      (* > *)
  | LPAREN      (* ( *)
  | RPAREN      (* ) *)
  | LBRACE      (* { *)
  | RBRACE      (* } *)
  | LBRACKET    (* [ *)
  | RBRACKET    (* ] *)
  | SEMICOLON   (* ; *)
  | COMMA       (* , *)
  | COLON       (* : *)
  
  (* Special *)
  | EOF
  | NEWLINE

(* Token with position information *)
type positioned_token = {
  token : token;
  position : position;
}

(* Main tokenization function *)
let tokenize (source : string) (filename : string) : positioned_token list =
  (* This function should:
     1. Take source code as a string
     2. Scan character by character
     3. Recognize patterns (keywords, operators, literals, identifiers)
     4. Return a list of positioned tokens
     5. Handle whitespace and comments
     6. Track line/column numbers for error reporting
  *)
  let len = String.length source in
  let pos = ref {line = 1; column = 1; filename = filename} in
  let tokens = ref [] in
  let i = ref 0 in
  
  (* Helper functions *)
  let current_char () = if !i < len then Some source.[!i] else None in
  
  let advance () = 
    if !i < len && source.[!i] = '\n' then
      pos := { !pos with line = !pos.line + 1; column = 1 }
    else
      pos := { !pos with column = !pos.column + 1 };
    i := !i + 1
  in
   
  let peek_char offset = 
    if !i + offset < len then Some source.[!i + offset] else None
  in
    let current_char_unwrapped () = source.[!i] in

  let add_token token = 
    (* This should:
       1. Create a positioned_token with the current position
       2. Add it to the tokens list
       3. Remember tokens is a ref, so use := to update it
    *)
    let positioned_token = { token = token; position = !pos } in
    tokens := positioned_token :: !tokens;
  in

  while !i < len do
    match current_char () with
    | Some ' ' | Some '\t'  -> advance ()
    | Some '\n' -> 
        add_token NEWLINE;
        advance ()
    | Some '/' -> 
        (* Handle comments and division *)
        (match peek_char 1 with
        | Some '/' -> 
            (* Line comment: skip until end of line *)
            advance (); (* Skip first / *)
            advance (); (* Skip second / *)
            while !i < len && current_char_unwrapped () <> '\n' do
              advance ()
            done
            (* Note: we don't advance past \n, let the newline case handle it *)
        | Some '*' -> 
            (* Block comment: skip until */ *)
            advance (); (* Skip / *)
            advance (); (* Skip * *)
            let rec skip_block_comment () =
              if !i >= len then
                failwith "Unterminated block comment"
              else if !i + 1 < len && 
                      current_char_unwrapped () = '*' && 
                      source.[!i + 1] = '/' then (
                advance (); (* Skip * *)
                advance ()  (* Skip / *)
              ) else (
                advance ();
                skip_block_comment ()
              )
            in
            skip_block_comment ()
        | _ -> 
            (* Just division *)
            add_token DIVIDE;
            advance ()
        )
    | Some '<' -> 
        (* Handle <, <<, <= *)
        (match peek_char 1 with
        | Some '<' -> 
            (* << operator *)
            add_token STREAM;
            advance (); (* Skip first < *)
            advance ()  (* Skip second < *)
        | Some '=' -> 
            (* <= operator *)
            add_token LESS_EQUAL;
            advance (); (* Skip < *)
            advance ()  (* Skip = *)
        | _ -> 
            (* Just < *)
            add_token LESS;
            advance ()
        )
    | Some '>' -> 
        (* Handle >, >= *)
        (match peek_char 1 with
        | Some '=' -> 
            (* >= operator *)
            add_token GREATER_EQUAL;
            advance (); (* Skip > *)
            advance ()  (* Skip = *)
        | _ -> 
            (* Just > *)
            add_token GREATER;
            advance ()
        )
    | Some '(' -> 
        add_token LPAREN;
        advance ()
    | Some ')' -> 
        add_token RPAREN;
        advance ()
    | Some '{' -> 
        add_token LBRACE;
        advance ()
    | Some '}' -> 
        add_token RBRACE;
        advance ()
    | Some '[' -> 
        add_token LBRACKET;
        advance ()
    | Some ']' -> 
        add_token RBRACKET;
        advance ()
    | Some ';' -> 
        add_token SEMICOLON;
        advance ()
    | Some ',' -> 
        add_token COMMA;
        advance ()
    | Some ':' -> 
        (* Handle : and :: *)
        (match peek_char 1 with
        | Some ':' -> 
            (* :: operator *)
            add_token SCOPE;
            advance (); (* Skip first : *)
            advance ()  (* Skip second : *)
        | _ -> 
            (* Just : *)
            add_token COLON;
            advance ()
        )
    | Some '=' -> 
        (* Handle = and == *)
        (match peek_char 1 with
        | Some '=' -> 
            (* == operator *)
            add_token EQUAL;
            advance (); (* Skip first = *)
            advance ()  (* Skip second = *)
        | _ -> 
            (* Just = *)
            add_token ASSIGN;
            advance ()
        )
    | Some '+' -> 
        (* TODO: Handle + and .+ *)
        add_token PLUS;
        advance ()
    | Some '-' -> 
        (* Handle -, -> *)
        (match peek_char 1 with
        | Some '>' -> 
            (* -> operator *)
            add_token ARROW;
            advance (); (* Skip - *)
            advance ()  (* Skip > *)
        | _ -> 
            (* Just - *)
            add_token MINUS;
            advance ()
        )
    | Some '*' -> 
        (* TODO: Handle * and .* *)
        add_token MULTIPLY;
        advance ()
    | Some '.' -> 
        (* Handle dot operators and dots *)
        (match peek_char 1 with
        | Some '+' -> 
            (* .+ operator *)
            add_token DOT_PLUS;
            advance (); (* Skip . *)
            advance ()  (* Skip + *)
        | Some '-' -> 
            (* .- operator *)
            add_token DOT_MINUS;
            advance (); (* Skip . *)
            advance ()  (* Skip - *)
        | Some '*' -> 
            (* .* operator *)
            add_token DOT_MULTIPLY;
            advance (); (* Skip . *)
            advance ()  (* Skip * *)
        | Some '/' -> 
            (* ./ operator *)
            add_token DOT_DIVIDE;
            advance (); (* Skip . *)
            advance ()  (* Skip / *)
        | Some c when c >= '0' && c <= '9' -> 
            (* This looks like a float starting with . like .5 *)
            (* We should handle this in the number parsing case *)
            (* For now, just skip the dot *)
            advance ()
        | _ -> 
            (* Just a dot - skip for now since we don't have a DOT token *)
            advance ()
        )
    | Some '!' -> 
        (* Handle ! and != *)
        (match peek_char 1 with
        | Some '=' -> 
            (* != operator *)
            add_token NOT_EQUAL;
            advance (); (* Skip ! *)
            advance ()  (* Skip = *)
        | _ -> 
            (* Just ! - skip for now since we don't have a NOT token *)
            advance ()
        )
    | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' -> 
        (* Collect identifier characters *)
        let buffer = Buffer.create 16 in
        while !i < len && (
          let curr = current_char_unwrapped () in
          (curr >= 'a' && curr <= 'z') || (curr >= 'A' && curr <= 'Z') || 
          (curr >= '0' && curr <= '9') || curr = '_'
        ) do 
          Buffer.add_char buffer (current_char_unwrapped ());
          advance ()
        done;
        
        let identifier = Buffer.contents buffer in
        let token = match identifier with
          | "class" -> CLASS
          | "public" -> PUBLIC
          | "private" -> PRIVATE
          | "if" -> IF
          | "or" -> OR
          | "for" -> FOR
          | "while" -> WHILE
          | "return" -> RETURN
          | "void" -> VOID
          | "int8" -> INT8
          | "uint8" -> UINT8
          | "int16" -> INT16
          | "uint16" -> UINT16
          | "int32" -> INT32
          | "uint32" -> UINT32
          | "int64" -> INT64
          | "uint64" -> UINT64
          | "float32" -> FLOAT32
          | "float64" -> FLOAT64
          | "string" -> STRING_TYPE
          | "bool" -> BOOL_TYPE
          | "true" -> BOOL true
          | "false" -> BOOL false
          | _ -> IDENTIFIER identifier
        in
        add_token token
    | Some c when (c >= '0' && c <= '9') -> 
        (* TODO: Handle numbers *)
        let buffer = Buffer.create 16 in
        while !i < len && (
          let curr = current_char_unwrapped () in
          (curr >= '0' && curr <= '9') 
        ) do 
          Buffer.add_char buffer (current_char_unwrapped ());
          advance ()
        done;
        let is_float = !i < len && current_char_unwrapped () = '.' && 
                       !i + 1 < len && (
                         let next_char = source.[!i + 1] in
                         next_char >= '0' && next_char <= '9'
                       ) in

        if is_float then (
          Buffer.add_char buffer '.';
          advance ();
          while !i < len && (
            let curr = current_char_unwrapped () in
            (curr >= '0' && curr <= '9')
          ) do 
            Buffer.add_char buffer (current_char_unwrapped ());
            advance ()
          done;

          let float_str = Buffer.contents buffer in
          let float_val = float_of_string float_str in
          add_token (FLOAT float_val)
        ) else (
          let int_str = Buffer.contents buffer in
          let int_val = int_of_string int_str in
          add_token (INTEGER int_val)
        )
    | Some '"' -> 
        (* Parse string literal *)
        advance (); (* Skip opening quote *)
        let buffer = Buffer.create 64 in
        let rec parse_string () =
          if !i >= len then
            failwith "Unterminated string literal"
          else
            match current_char_unwrapped () with
            | '"' -> 
                (* End of string *)
                advance (); (* Skip closing quote *)
                let str_content = Buffer.contents buffer in
                add_token (STRING str_content)
            | '\\' when !i + 1 < len -> 
                (* Escape sequence *)
                advance (); (* Skip backslash *)
                (match current_char_unwrapped () with
                | 'n' -> Buffer.add_char buffer '\n'
                | 't' -> Buffer.add_char buffer '\t'
                | 'r' -> Buffer.add_char buffer '\r'
                | '\\' -> Buffer.add_char buffer '\\'
                | '"' -> Buffer.add_char buffer '"'
                | c -> Buffer.add_char buffer c (* Other escapes just add the char *)
                );
                advance ();
                parse_string ()
            | c -> 
                (* Regular character *)
                Buffer.add_char buffer c;
                advance ();
                parse_string ()
        in
        parse_string ()

    | Some _ -> 
        (* Unknown character - skip for now *)
        advance ()
    | None -> 
        (* End of input *)
        ()
  done;
  
  add_token EOF;
      
      List.rev !tokens

(* Helper function to convert token to string (for debugging) *)
let string_of_token (token : token) : string =
  (* This function should convert each token variant to a readable string
     Useful for debugging and error messages
  *)
  match token with 
  | INTEGER i -> "INTEGER(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | STRING s -> "STRING(" ^ s ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | IDENTIFIER s -> "IDENTIFIER(" ^ s ^ ")"
  | CLASS -> "CLASS"
  | PUBLIC -> "PUBLIC"
  | PRIVATE -> "PRIVATE"
  | IF -> "IF"
  | OR -> "OR"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | RETURN -> "RETURN"
  | VOID -> "VOID"
  | INT8 -> "INT8"
  | UINT8 -> "UINT8"
  | INT16 -> "INT16"
  | UINT16 -> "UINT16"
  | INT32 -> "INT32"
  | UINT32 -> "UINT32"
  | INT64 -> "INT64"
  | UINT64 -> "UINT64"
  | FLOAT32 -> "FLOAT32"
  | FLOAT64 -> "FLOAT64"
  | STRING_TYPE -> "STRING_TYPE"
  | BOOL_TYPE -> "BOOL_TYPE"
  | C_PREFIX -> "C_PREFIX"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULTIPLY -> "MULTIPLY"
  | DIVIDE -> "DIVIDE"
  | DOT_PLUS -> "DOT_PLUS"
  | DOT_MINUS -> "DOT_MINUS"
  | DOT_MULTIPLY -> "DOT_MULTIPLY"
  | DOT_DIVIDE -> "DOT_DIVIDE"
  | EQUAL -> "EQUAL"
  | NOT_EQUAL -> "NOT_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | ARROW -> "ARROW"
  | SCOPE -> "SCOPE"
  | STREAM -> "STREAM"
  | LANGLE -> "LANGLE"
  | RANGLE -> "RANGLE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"
