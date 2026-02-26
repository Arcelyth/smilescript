open Lexer
open Printf
open Smc

type expr = 
  | Binary of expr * token * expr
  | Unary of token * expr
  | Literal of token_type
  | Grouping of expr
  | Variable of token

type stmt =
  | PrintStmt of expr
  | ExprStmt of expr
  | VarStmt of string * expr

type program = stmt list

exception ParseError of token * string

let parse_error token message state =
  let where = if token.kind = EOF then " at end" else " at '" ^ token.lexeme ^ "'" in
  report token.line where message state

let rec synchronize = function
  | [] -> []
  | {kind=Semicolon; _} :: rest -> rest
  | {kind=Class|Fun|Var|For|If|While|Print|Return; _} :: _ as tks -> tks
  | _ :: rest -> synchronize rest

let consume tokens expected_kind msg =
  match tokens with
  | {kind; _} :: rest when kind = expected_kind -> rest
  | t :: _ -> raise (ParseError (t, msg))
  | [] -> failwith "Unexpected EOF"


let rec declaration = function 
  {kind=Var; _} :: res -> varDeclaration res
  | tks -> statement tks

and varDeclaration = function 
  {kind=Identifier name; _} :: tokens2 -> 
    (match tokens2 with 
    | {kind=Equal; _} :: rest -> 
        let e, rest2 = expression rest in
        let rest3 = consume rest2 Semicolon "Expect ';' after variable declaration." in
        (VarStmt (name, e), rest3)
    | _ -> 
        let rest2 = consume tokens2 Semicolon "Expect ';' after variable declaration." in
        (VarStmt (name, Literal Nil), rest2))
  | t :: _ -> raise (ParseError (t, "Expect variable name"))
  | [] -> failwith "Unexpected EOF"

and statement = function
  {kind=Print; _} :: rest -> print_statement rest
  | tks -> expr_statement tks
 
and print_statement tokens =
  let e, tokens = expression tokens in 
  let tokens = consume tokens Semicolon "Expect ';' after value." in
  PrintStmt e, tokens

and expr_statement tokens = 
  let e, tokens = expression tokens in 
  let tokens = consume tokens Semicolon "Expect ';' after expression." in
  ExprStmt e, tokens

and expression tokens = 
  equality tokens 

and equality tokens  =
  let expr, tokens = comparison tokens  in
  let rec loop expr = function
    | {kind=Bang_equal | Equal_equal; _ } as op :: rest -> 
        let right, tokens = comparison rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in
  loop expr tokens

and comparison tokens  =
  let expr, tokens = term tokens  in
  let rec loop expr = function 
    | {kind=Greater_equal | Greater | Less_equal | Less; _ } as op :: rest -> 
        let right, tokens = term rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and term tokens  =
  let expr, tokens = factor tokens  in
  let rec loop expr = function 
    | {kind=Minus | Plus; _ } as op :: rest -> 
        let right, tokens = factor rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and factor tokens  =
  let expr, tokens = unary tokens  in
  let rec loop expr = function 
    | {kind=Slash | Star; _ } as op :: rest -> 
        let right, tokens = unary rest  in
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and unary tokens  =
  match tokens with
  | {kind=Bang | Minus; _ } as op :: rest -> 
      let right, tokens = unary rest  in
      Unary (op, right), tokens
  | _ -> primary tokens 

and primary tokens  =
  match tokens with
  | {kind=Number _; _} as t :: rest -> Literal t.kind, rest 
  | {kind=String _; _} as t :: rest -> Literal t.kind, rest 
  | {kind=True; _} as t :: rest -> Literal t.kind, rest 
  | {kind=False; _} as t :: rest -> Literal t.kind, rest 
  | {kind=Nil; _} as t :: rest -> Literal t.kind, rest 
  | {kind=Identifier _; _} as t :: rest -> Variable t, rest
  | {kind=Left_paren; _} as tok :: rest -> 
      let expr, tokens = expression rest in
      (match tokens with 
       | {kind=Right_paren; _} :: rest2 -> Grouping expr, rest2
       | t :: _ -> raise (ParseError (t, "Expect ')' after expression."))
       | [] -> raise (ParseError (tok, "Expect ')' after expression.")))
  | t :: _ -> raise (ParseError (t, "Expect expression."))
  | [] -> failwith "Unexpected end of input: no tokens to parse."

let parse tokens state =
  let rec loop acc current_tokens =
    match current_tokens with
    | [] | {kind = EOF; _} :: _ -> List.rev acc
    | _ ->
        try
          let stmt, next_tokens = declaration current_tokens in
          loop (stmt :: acc) next_tokens
        with
        | ParseError (tk, msg) ->
            parse_error tk msg state;
            (* enter panic mod *)
            loop acc (synchronize current_tokens)
  in
  loop [] tokens

let rec print_expr = function 
  | Binary (left, op, right) -> 
      parenthesize op.lexeme [left; right]
  | Unary (op, right) -> 
      parenthesize op.lexeme [right]
  | Grouping e -> 
      parenthesize "group" [e] 
  | Literal ty -> 
      string_of_literal ty
  | Variable t -> 
      sprintf "(var %s)" t.lexeme

and string_of_literal = function 
  | Number n -> 
      let s = string_of_float n in
      if String.ends_with ~suffix:"." s then s ^ "0" else s
  | String s -> s
  | Boolean b -> string_of_bool b
  | Nil -> "nil"
  | _ -> ""

and parenthesize name exprs = 
  let subs = List.map print_expr exprs in
  sprintf "(%s %s)" name (String.concat " " subs)
