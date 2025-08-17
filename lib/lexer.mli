(* Lexer interface - exposes types and functions *)

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
val tokenize : string -> string -> positioned_token list

(* Helper function to convert token to string (for debugging) *)
val string_of_token : token -> string
