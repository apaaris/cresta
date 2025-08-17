(* LLVM Code Generator Implementation *)

open Parser
open Llvm

(* LLVM context and state *)
type llvm_context = {
  context : Llvm.llcontext;
  the_module : Llvm.llmodule;
  builder : Llvm.llbuilder;
  mutable named_values : (string, Llvm.llvalue) Hashtbl.t;
  mutable class_types : (string, Llvm.lltype) Hashtbl.t;
}

(* Code generation errors *)
type codegen_error = {
  message : string;
  location : string;
}

exception CodegenError of codegen_error

(* Create a new LLVM context *)
let create_context module_name =
  let context = global_context () in
  let the_module = create_module context module_name in
  let builder = builder context in
  {
    context;
    the_module;
    builder;
    named_values = Hashtbl.create 32;
    class_types = Hashtbl.create 16;
  }

(* Convert Cresta types to LLVM types *)
let rec cresta_type_to_llvm ctx = function
  | Int8 -> i8_type ctx.context
  | UInt8 -> i8_type ctx.context  (* LLVM doesn't distinguish signed/unsigned at type level *)
  | Int16 -> i16_type ctx.context
  | UInt16 -> i16_type ctx.context
  | Int32 -> i32_type ctx.context
  | UInt32 -> i32_type ctx.context
  | Int64 -> i64_type ctx.context
  | UInt64 -> i64_type ctx.context
  | Float32 -> float_type ctx.context
  | Float64 -> double_type ctx.context
  | Bool -> i1_type ctx.context
  | String -> pointer_type (i8_type ctx.context)  (* char* for now *)
  | Complex _ -> 
      (* For now, represent complex as a struct with two components *)
      raise (CodegenError { message = "Complex types not yet implemented"; location = "type conversion" })
  | Array (element_type, dimensions) -> 
      (* For now, handle 1D arrays only *)
      (match dimensions with
       | [size] -> 
           let element_llvm_type = cresta_type_to_llvm ctx element_type in
           array_type element_llvm_type size
       | _ -> 
           raise (CodegenError { message = "Multi-dimensional arrays not yet implemented"; location = "type conversion" }))
  | UserDefined class_name -> 
      (* Look up the class type *)
      (try 
        Hashtbl.find ctx.class_types class_name
      with Not_found -> 
        raise (CodegenError { message = "Unknown class type: " ^ class_name; location = "type conversion" }))

(* Generate code for expressions *)
let rec generate_expression ctx = function
  | IntLiteral i -> const_int (i32_type ctx.context) i
  | FloatLiteral f -> const_float (double_type ctx.context) f
  | BoolLiteral b -> const_int (i1_type ctx.context) (if b then 1 else 0)
  | StringLiteral s -> 
      (* Create a global string constant *)
      let str_const = const_stringz ctx.context s in
      let global_str = define_global ("str_" ^ (string_of_int (Hashtbl.hash s))) str_const ctx.the_module in
      build_gep global_str [| const_int (i32_type ctx.context) 0; const_int (i32_type ctx.context) 0 |] "str_ptr" ctx.builder
  | Variable name -> 
      (try 
        (* Get the pointer to the variable *)
        let var_ptr = Hashtbl.find ctx.named_values name in
        (* For arrays, return the pointer directly (don't load) *)
        (* For scalars, load the value *)
        let var_type = type_of var_ptr in
        if classify_type var_type = TypeKind.Pointer then
          let pointee_type = element_type var_type in
          if classify_type pointee_type = TypeKind.Array then
            (* This is an array - return pointer to first element *)
            let zero = const_int (i32_type ctx.context) 0 in
            build_gep var_ptr [| zero; zero |] "arrayptr" ctx.builder
          else
            (* This is a regular variable - load the value *)
            build_load var_ptr name ctx.builder
        else
          (* Not a pointer, return as-is *)
          var_ptr
      with Not_found -> 
        raise (CodegenError { message = "Unknown variable: " ^ name; location = "expression generation" }))
  | BinaryOp (left, op, right) ->
      let left_val = generate_expression ctx left in
      let right_val = generate_expression ctx right in
      generate_binary_op ctx left_val op right_val
  | MemberAccess (obj_expr, member_name) -> 
      (* Generate code for the object *)
      let obj_ptr = generate_expression ctx obj_expr in
      
      (* For now, assume it's a field access - we need field index *)
      (* This is simplified - in a real implementation, we'd need to track field positions *)
      let zero = const_int (i32_type ctx.context) 0 in
      let field_index = const_int (i32_type ctx.context) 0 in  (* TODO: Calculate actual field index *)
      
      (* Use getelementptr to get field address *)
      let field_ptr = build_gep obj_ptr [| zero; field_index |] ("field_" ^ member_name) ctx.builder in
      
      (* Load the field value *)
      build_load field_ptr member_name ctx.builder
  | MethodCall (obj, _method_name, args) -> 
      (* For now, treat this as a function call if obj is a Variable *)
      (match obj with
       | Variable func_name ->
           (* This is a function call: func_name(args) *)
           let arg_values = List.map (generate_expression ctx) args in
           let func_opt = lookup_function func_name ctx.the_module in
           let func = match func_opt with
             | Some f -> f
             | None -> raise (CodegenError { message = "Unknown function: " ^ func_name; location = "function call" })
           in
           build_call func (Array.of_list arg_values) "calltmp" ctx.builder
       | _ -> 
           raise (CodegenError { message = "Method calls on objects not yet implemented"; location = "expression generation" }))
  | ArrayAccess (array_expr, indices) -> 
      (* For now, handle 1D array access only *)
      (match indices with
       | [index_expr] ->
           (* Generate code for the array and index *)
           let array_ptr = generate_expression ctx array_expr in
           let index_val = generate_expression ctx index_expr in
           
           (* Use getelementptr to get the address of array[index] *)
           let gep = build_gep array_ptr [| index_val |] "arrayidx" ctx.builder in
           
           (* Load the value from that address *)
           build_load gep "arrayval" ctx.builder
           
       | _ -> 
           raise (CodegenError { message = "Multi-dimensional array access not yet implemented"; location = "expression generation" }))

(* Generate binary operations *)
and generate_binary_op ctx left op right =
  match op with
  | "+" -> 
      (* Determine if we're dealing with integers or floats *)
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fadd left right "addtmp" ctx.builder
      else
        build_add left right "addtmp" ctx.builder
  | "-" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fsub left right "subtmp" ctx.builder
      else
        build_sub left right "subtmp" ctx.builder
  | "*" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fmul left right "multmp" ctx.builder
      else
        build_mul left right "multmp" ctx.builder
  | "/" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fdiv left right "divtmp" ctx.builder
      else
        build_sdiv left right "divtmp" ctx.builder
  | "==" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.Oeq left right "eqtmp" ctx.builder
      else
        build_icmp Icmp.Eq left right "eqtmp" ctx.builder
  | "!=" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.One left right "neqtmp" ctx.builder
      else
        build_icmp Icmp.Ne left right "neqtmp" ctx.builder
  | "<" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.Olt left right "lttmp" ctx.builder
      else
        build_icmp Icmp.Slt left right "lttmp" ctx.builder
  | ">" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.Ogt left right "gttmp" ctx.builder
      else
        build_icmp Icmp.Sgt left right "gttmp" ctx.builder
  | "<=" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.Ole left right "letmp" ctx.builder
      else
        build_icmp Icmp.Sle left right "letmp" ctx.builder
  | ">=" -> 
      if type_of left = double_type ctx.context || type_of right = double_type ctx.context then
        build_fcmp Fcmp.Oge left right "getmp" ctx.builder
      else
        build_icmp Icmp.Sge left right "getmp" ctx.builder
  | _ -> raise (CodegenError { message = "Unknown binary operator: " ^ op; location = "binary operation" })

(* Placeholder for statement generation - we'll implement this next *)
let rec generate_statement ctx = function
  | VarDecl (var_type, var_name, init_expr) ->
      (* Step 1: Convert Cresta type to LLVM type *)
      let llvm_type = cresta_type_to_llvm ctx var_type in
      
      (* Step 2: Allocate stack space for the variable *)
      let alloca_inst = build_alloca llvm_type var_name ctx.builder in
      
      (* Step 3: Store the allocated pointer in our symbol table *)
      Hashtbl.replace ctx.named_values var_name alloca_inst;
      
      (* Step 4: If there's an initial value, generate it and store it *)
      (match init_expr with
       | Some expr -> 
           let init_val = generate_expression ctx expr in
           ignore (build_store init_val alloca_inst ctx.builder)
       | None -> () (* No initialization *)
      );
      
      (* Step 5: Return the alloca instruction *)
      Some alloca_inst
      
  | Assignment (var_name, expr) ->
      (* Step 1: Look up the variable pointer in our symbol table *)
      (try 
        let var_ptr = Hashtbl.find ctx.named_values var_name in
        
        (* Step 2: Generate code for the expression *)
        let value = generate_expression ctx expr in
        
        (* Step 3: Store the value to the variable's memory location *)
        let store_inst = build_store value var_ptr ctx.builder in
        
        (* Step 4: Return the store instruction *)
        Some store_inst
        
      with Not_found -> 
        raise (CodegenError { message = "Unknown variable: " ^ var_name; location = "assignment" }))
      
  | ExpressionStmt expr ->
      (* Just generate the expression and return it *)
      let value = generate_expression ctx expr in
      Some value
      
  | IfStmt (condition, then_stmts, else_stmts_opt) ->
      (* Step 1: Generate the condition expression *)
      let cond_val = generate_expression ctx condition in
      
      (* Step 2: Get the current function we're building inside *)
      let current_func = block_parent (insertion_block ctx.builder) in
      
      (* Step 3: Create basic blocks *)
      let then_bb = append_block ctx.context "then" current_func in
      let merge_bb = append_block ctx.context "ifcont" current_func in
      
      (* Create else block if we have else statements *)
      let else_bb = match else_stmts_opt with
        | Some _ -> append_block ctx.context "else" current_func
        | None -> merge_bb (* If no else, jump directly to merge *)
      in
      
      (* Step 4: Build conditional branch *)
      ignore (build_cond_br cond_val then_bb else_bb ctx.builder);
      
      (* Step 5: Generate then statements *)
      position_at_end then_bb ctx.builder;
      List.iter (fun stmt -> ignore (generate_statement ctx stmt)) then_stmts;
      
      (* Build branch to merge block *)
      ignore (build_br merge_bb ctx.builder);
      
      (* Step 6: Generate else statements if they exist *)
      (match else_stmts_opt with
       | Some else_stmts ->
           position_at_end else_bb ctx.builder;
           List.iter (fun stmt -> ignore (generate_statement ctx stmt)) else_stmts;
           (* Build branch to merge *)
           ignore (build_br merge_bb ctx.builder)
       | None -> () (* No else block to generate *)
      );
      
      (* Step 7: Position builder at merge block for subsequent instructions *)
      position_at_end merge_bb ctx.builder;
      
      (* Return None since if statements don't produce values *)
      None
      
  | Block stmt_list ->
      (* Generate each statement in the block *)
      let results = List.map (generate_statement ctx) stmt_list in
      (* Return the last generated value, or None if the list is empty *)
      (match List.rev results with
       | [] -> None
       | last :: _ -> last)
      
  | ReturnStmt expr_opt ->
      (match expr_opt with
       | Some expr -> 
           (* Generate the return expression and return it *)
           let return_val = generate_expression ctx expr in
           let ret_inst = build_ret return_val ctx.builder in
           Some ret_inst
       | None -> 
           (* Return void *)
           let ret_inst = build_ret_void ctx.builder in
           Some ret_inst)
      
  | WhileLoop (condition, body_stmts) ->
      (* Step 1: Get the current function *)
      let current_func = block_parent (insertion_block ctx.builder) in
      
      (* Step 2: Create basic blocks *)
      let loop_cond = append_block ctx.context "loop_cond" current_func in
      let loop_body = append_block ctx.context "loop_body" current_func in
      let loop_end = append_block ctx.context "loop_end" current_func in
      
      (* Step 3: Branch to condition check *)
      ignore (build_br loop_cond ctx.builder);
      
      (* Step 4: Generate condition check *)
      position_at_end loop_cond ctx.builder;
      let cond_val = generate_expression ctx condition in
      ignore (build_cond_br cond_val loop_body loop_end ctx.builder);
      
      (* Step 5: Generate loop body *)
      position_at_end loop_body ctx.builder;
      List.iter (fun stmt -> ignore (generate_statement ctx stmt)) body_stmts;
      
      (* Step 6: Branch back to condition *)
      ignore (build_br loop_cond ctx.builder);
      
      (* Step 7: Position builder at loop end for subsequent instructions *)
      position_at_end loop_end ctx.builder;
      
      (* Return None since loops don't produce values *)
      None
      
  | ForLoop (_init_stmt, _condition, _update_expr, _body_stmts) ->
      (* TODO: You implement this later! *)
      (* This is more complex - for now focus on while loops *)
      failwith "ForLoop not implemented yet"
      
  | FunctionDecl func_decl ->
      (* Generate function directly here to avoid forward reference *)
      let func_val = generate_function_impl ctx func_decl in
      Some func_val
      
  | ClassDecl class_decl ->
      (* Step 1: Extract fields from class members *)
      let fields = List.fold_left (fun acc member ->
        match member with
        | Field (_, field_type, _) -> field_type :: acc
        | Method _ -> acc  (* Methods don't take up space in the struct *)
      ) [] class_decl.members |> List.rev in
      
      (* Step 2: Convert field types to LLVM types *)
      let field_types = Array.of_list (List.map (cresta_type_to_llvm ctx) fields) in
      
      (* Step 3: Create struct type for the class *)
      let struct_type = struct_type ctx.context field_types in
      
      (* Step 4: Store the class type *)
      Hashtbl.replace ctx.class_types class_decl.class_name struct_type;
      
      (* Step 5: Generate methods as functions with 'this' parameter *)
      List.iter (fun member ->
        match member with
        | Field _ -> () (* Fields are handled above *)
        | Method method_decl ->
            (* Add 'this' parameter as first parameter *)
            let this_param = { 
              param_type = UserDefined class_decl.class_name; 
              param_name = "this" 
            } in
            let method_with_this = {
              method_decl with 
              parameters = this_param :: method_decl.parameters;
              func_name = class_decl.class_name ^ "_" ^ method_decl.func_name
            } in
            ignore (generate_function_impl ctx method_with_this)
      ) class_decl.members;
      
      (* Return the struct type as an LLVM value (for consistency) *)
      Some (undef struct_type)
(* Placeholder for function generation - we'll implement this next *)
and generate_function_impl ctx func_decl =
  (* Step 1: Convert parameter types to LLVM types *)
  let param_types = Array.of_list (List.map (fun param -> cresta_type_to_llvm ctx param.param_type) func_decl.parameters) in
  
  (* Step 2: Convert return type to LLVM type *)
  let return_type = match func_decl.return_type with
    | Some ret_type -> cresta_type_to_llvm ctx ret_type
    | None -> void_type ctx.context
  in
  
  (* Step 3: Create function type *)
  let func_type = function_type return_type param_types in
  
  (* Step 4: Define the function in the module *)
  let llvm_func = define_function func_decl.func_name func_type ctx.the_module in
  
  (* Step 5: Create entry basic block *)
  let entry_bb = append_block ctx.context "entry" llvm_func in
  position_at_end entry_bb ctx.builder;
  
  (* Step 6: Save current named_values and create new scope for function *)
  let saved_values = Hashtbl.copy ctx.named_values in
  Hashtbl.clear ctx.named_values;
  
  (* Step 7: Set up parameters - allocate space and store parameter values *)
  let llvm_params = params llvm_func in
  List.iteri (fun i param ->
    let param_val = llvm_params.(i) in
    set_value_name param.param_name param_val;
    let alloca = build_alloca (cresta_type_to_llvm ctx param.param_type) param.param_name ctx.builder in
    ignore (build_store param_val alloca ctx.builder);
    Hashtbl.add ctx.named_values param.param_name alloca
  ) func_decl.parameters;
  
  (* Step 8: Generate function body *)
  List.iter (fun stmt -> ignore (generate_statement ctx stmt)) func_decl.body;
  
  (* Step 9: Add implicit return for void functions if needed *)
  (match func_decl.return_type with
   | None -> 
       (* For void functions, always add a return void at the end *)
       ignore (build_ret_void ctx.builder)
   | Some _ -> 
       (* For non-void functions, assume they have explicit returns *)
       (* TODO: Add proper terminator checking later *)
       ()
  );
  
  (* Step 10: Restore previous scope *)
  Hashtbl.clear ctx.named_values;
  Hashtbl.iter (Hashtbl.add ctx.named_values) saved_values;
  
  (* Step 11: Return the generated function *)
  llvm_func

(* Public wrapper for function generation *)
let generate_function ctx func_decl = generate_function_impl ctx func_decl

(* Generate code for the entire program *)
let generate_program ctx program =
  List.iter (fun stmt -> ignore (generate_statement ctx stmt)) program;
  ctx.the_module

(* Utility functions *)
let print_module llmodule =
  print_endline (string_of_llmodule llmodule)

let write_module_to_file llmodule filename =
  (* For now, just write to text format - we'll add bitcode later *)
  let content = string_of_llmodule llmodule in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let optimize_module _llmodule =
  (* TODO: Add optimizations later when we have the right libraries *)
  Printf.printf "Optimization not yet implemented\n"

(* Error handling *)
let print_codegen_error error =
  Printf.printf "Code generation error at %s: %s\n" error.location error.message
