open Parser
open Smc

exception RuntimeError of Lexer.token * string
exception BreakException
exception ContinueException
exception ReturnException of value

let rec env_exist env key =
  if Hashtbl.mem env.values key then true
  else match env.enclosing with
       | Some parent -> env_exist parent key
       | None -> false

let env_define env name v =
  Hashtbl.replace env.values name v

let rec env_get env name tk =
  if Hashtbl.mem env.values name then
    Hashtbl.find env.values name
  else
    match env.enclosing with
    | Some parent -> env_get parent name tk
    | None -> raise (RuntimeError (tk, "Undefined variable '" ^ name ^ "'."))

let rec env_bind env name v tk =
  if Hashtbl.mem env.values name then
    Hashtbl.replace env.values name v
  else
    match env.enclosing with
    | Some parent -> env_bind parent name v tk
    | None -> raise (RuntimeError (tk, "Undefined variable '" ^ name ^ "'."))

(* define native functions *)

let native_functions = [
  ("clock", 0, fun _ _ -> VNum (Unix.gettimeofday ()));
  
  ("type", 1, fun _ args -> 
      match List.hd args with
      | VNum _ -> VStr "number"
      | VStr _ -> VStr "string"
      | VBool _ -> VStr "boolean"
      | VNil -> VStr "nil"
      | VCallable _ -> VStr "function"
  );
]
(* end of define native functions *)

  
let runtime_error token msg state = 
  Printf.printf "%s\n[line %d]\n" msg token.Lexer.line;
  state.had_runtime_err <- true

let string_of_value = function
  | VNum n -> 
      let str = string_of_float n in
      (match String.ends_with ~suffix:"." str with 
      | true -> String.sub str 0 (String.length str - 1) 
      | false -> str)
  | VStr s -> s
  | VBool b -> string_of_bool b
  | VNil -> "nil"
  | VCallable _ -> "<fn>"

let is_equal a b =
  match (a, b) with
  | (VNil, VNil) -> true
  | (VNil, _) -> false
  | (VNum n1, VNum n2) -> n1 = n2
  | (VStr s1, VStr s2) -> s1 = s2
  | (VBool b1, VBool b2) -> b1 = b2
  | _ -> false

let rec execute (state : state) = function
  | IfStmt (cond, then_br, else_br) -> 
      (match is_truthy @@ evaluate_expr cond state with 
      | true -> 
          execute state then_br 
      | false -> 
          execute state else_br)
  | WhileStmt (cond, body, inc) -> 
      let rec loop () = 
        match is_truthy (evaluate_expr cond state) with 
        | true -> 
            (try 
              execute state body;
              Option.iter (execute state) inc;
              loop ()
            with 
            | BreakException -> ()
            | ContinueException -> 
                Option.iter (execute state) inc;
                loop ())
        | false -> ()
      in
      loop ()
  | PrintStmt expr -> 
      let v = evaluate_expr expr state in
      print_endline (string_of_value v)
  | ExprStmt expr -> 
      ignore (evaluate_expr expr state)
  | VarStmt (name, expr) ->
      let v = evaluate_expr expr state in 
      ignore (env_define state.cur_env name v )
  | Block stmts -> 
      let new_env = { values = Hashtbl.create 16; enclosing = Some state.cur_env } in
      execute_block stmts new_env state
  | BreakStmt -> 
      raise BreakException
  | ContinueStmt -> 
      raise ContinueException
  | FuncStmt (name, params, body) ->
      let closure = state.cur_env in 
      let func_value = VCallable {
        arity = List.length params;
        call = (fun state args ->
          let env = { 
            values = Hashtbl.create (List.length params); 
            enclosing = Some closure 
          } in
          List.iter2 (fun param_name arg_val ->
            ignore (env_define env param_name arg_val)
          ) params args;
          try
            execute_block body env state;
            VNil 
          with
          | ReturnException value -> value 
        );
      } in
      ignore (env_define state.cur_env name func_value)
  | ReturnStmt (_, e) -> 
      let value = evaluate_expr e state in
      raise (ReturnException value)

and execute_block stmts next_env state = 
  let prev_env = state.cur_env in 
  try 
    state.cur_env <- next_env;
    List.iter (execute state) stmts;
    state.cur_env <- prev_env 
  with e ->
    state.cur_env <- prev_env; 
    raise e

and evaluate_expr expr state = match expr with
  Binary (l, tk, r) -> 
    let l_expr = evaluate_expr l state in
    let r_expr = evaluate_expr r state in
    (match tk.kind with
    | Minus -> check_num_op tk l_expr r_expr (fun n1 n2 -> VNum (n1 -. n2))
    | Slash -> check_num_op tk l_expr r_expr (fun n1 n2 -> VNum (n1 /. n2))
    | Star  -> check_num_op tk l_expr r_expr (fun n1 n2 -> VNum (n1 *. n2))
    | Plus  -> 
        (match (l_expr, r_expr) with
        | (VNum n1, VNum n2) -> VNum (n1 +. n2)
        | (VStr s1, VStr s2) -> VStr (s1 ^ s2)
        | _ -> raise (RuntimeError (tk, "Operands must be two numbers or two strings.")))
    | Greater -> check_num_op tk l_expr r_expr (fun n1 n2 -> VBool (n1 > n2))
    | Greater_equal -> check_num_op tk l_expr r_expr (fun n1 n2 -> VBool (n1 >= n2))
    | Less -> check_num_op tk l_expr r_expr (fun n1 n2 -> VBool (n1 < n2))
    | Less_equal -> check_num_op tk l_expr r_expr (fun n1 n2 -> VBool (n1 <= n2))
    | Bang_equal -> VBool (not (is_equal l_expr r_expr))
    | Equal_equal -> VBool (is_equal l_expr r_expr)
    | _ -> failwith "Internal error: unexpected binary operator")

  | Logical (l_expr, op, r_expr) -> 
      let left_v = evaluate_expr l_expr state in
      (match op with 
      | {kind=Or; _} -> 
        if (is_truthy left_v) then left_v
        else evaluate_expr r_expr state
      | _ -> if (not (is_truthy left_v)) then left_v
        else evaluate_expr r_expr state)

  | Unary (tk, e) -> 
      let r = evaluate_expr e state in
      (match tk.kind with 
      | Minus -> 
          (match r with
          | VNum n -> VNum (-. n)
          | _ -> raise (RuntimeError (tk, "Operand must be a number.")))
      | Bang -> VBool (not (is_truthy r))
      | _ -> failwith "Internal error: unexpected unary operator")

  | Literal (Number n) -> VNum n
  | Literal (String s) -> VStr s
  | Literal (Boolean b)-> VBool b
  | Literal Nil -> VNil
  | Literal _ -> failwith "Internal error: unexpected literal"
  | Variable t -> env_get state.cur_env t.lexeme t 
  | Assign (t, value) -> 
      let v = evaluate_expr value state in 
      if env_exist state.cur_env t.lexeme then (
        env_bind state.cur_env t.lexeme v t;
        v
      ) else (
        raise (RuntimeError (t, "Undefined variable '" ^ t.lexeme ^ "'.")))

  | Grouping e -> 
      evaluate_expr e state
  | Call (e, tk, args_exprs) -> 
      let callee_value = evaluate_expr e state in
      
      let args_values = List.map (fun arg -> evaluate_expr arg state) args_exprs in
      
      match callee_value with
      | VCallable callable ->
          if List.length args_values <> callable.arity then
            raise (RuntimeError (tk, Printf.sprintf "Expected %d arguments but got %d." callable.arity (List.length args_values)))
          else
            callable.call state args_values
      | _ -> 
          raise (RuntimeError (tk, "Can only call functions and classes."))

and is_truthy = function 
  | VNil | VBool false -> false
  | _ -> true

and check_num_op tk left right f =
  match (left, right) with
  | VNum n1, VNum n2 -> f n1 n2
  | _ -> raise (RuntimeError (tk, "Operands must be numbers."))

let interpret stmts state =
  List.iter (fun (name, arity, f) ->
    let callable = {
      arity = arity;
      call = f;
    } in
    ignore (env_define state.cur_env name (VCallable callable))
  ) native_functions;

  try
    List.iter (execute state) stmts
  with
  | RuntimeError (tk, msg) -> runtime_error tk msg state
  | BreakException -> 
      runtime_error {kind=EOF; lexeme=""; line=0} "Unexpected 'break' outside of loop." state
  | ContinueException -> 
      runtime_error {kind=EOF; lexeme=""; line=0} "Unexpected 'continue' outside of loop." state



  
