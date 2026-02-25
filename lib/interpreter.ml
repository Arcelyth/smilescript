open Parser
open Error

exception RuntimeError of Lexer.token * string

let runtime_error token msg state = 
  Printf.printf "%s\n[line %d]\n" msg token.Lexer.line;
  state.had_runtime_err <- true
  
(* value type in runtime *)
type value = 
  | VNum of float
  | VStr of string
  | VBool of bool
  | VNil

let string_of_value = function
  | VNum n -> 
      let str = string_of_float n in
      (match String.ends_with ~suffix:"." str with 
      | true -> String.sub str 0 (String.length str - 1) 
      | false -> str)
  | VStr s -> s
  | VBool b -> string_of_bool b
  | VNil -> "nil"

let is_equal a b =
  match (a, b) with
  | (VNil, VNil) -> true
  | (VNil, _) -> false
  | (VNum n1, VNum n2) -> n1 = n2
  | (VStr s1, VStr s2) -> s1 = s2
  | (VBool b1, VBool b2) -> b1 = b2
  | _ -> false

let rec execute = function
  | PrintStmt expr -> 
      let v = evaluate_expr expr in
      print_endline (string_of_value v)
  | ExprStmt expr -> 
      ignore (evaluate_expr expr)

and evaluate_expr = function
  Binary (l, tk, r) -> 
    let l_expr = evaluate_expr l in
    let r_expr = evaluate_expr r in
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

  | Unary (tk, e) -> 
      let r = evaluate_expr e in
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
  | Grouping e -> 
      evaluate_expr e

  

and is_truthy = function 
  | VNil | VBool false -> false
  | _ -> true

and check_num_op tk left right f =
  match (left, right) with
  | VNum n1, VNum n2 -> f n1 n2
  | _ -> raise (RuntimeError (tk, "Operands must be numbers."))

let interpret stmts state = 
  try
    List.iter execute stmts
  with
    RuntimeError (tk, msg) -> runtime_error tk msg state




  
