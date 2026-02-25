open Lexer

exception ParseError

let parse_error token message state =
  let where = if token.kind = EOF then " at end" else " at '" ^ token.lexeme ^ "'" in
  Error.report token.line where message state;
  raise ParseError

type expr = 
  | Binary of expr * token * expr
  | Unary of token * expr
  | Literal of token_type
  | Grouping of expr

let rec expression tokens state = 
  equality tokens state

and equality tokens state =
  let expr, tokens = comparison tokens state in
  let rec loop expr = function
    | {kind=Bang_equal | Equal_equal; _ } as op :: rest -> 
        let right, tokens = comparison rest state in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in
  loop expr tokens

and comparison tokens state =
  let expr, tokens = term tokens state in
  let rec loop expr = function 
    | {kind=Greater_equal | Greater | Less_equal | Less; _ } as op :: rest -> 
        let right, tokens = term rest state in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and term tokens state =
  let expr, tokens = factor tokens state in
  let rec loop expr = function 
    | {kind=Minus | Plus; _ } as op :: rest -> 
        let right, tokens = factor rest state in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and factor tokens state =
  let expr, tokens = unary tokens state in
  let rec loop expr = function 
    | {kind=Slash | Star; _ } as op :: rest -> 
        let right, tokens = unary rest state in
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and unary tokens state =
  match tokens with
  | {kind=Bang | Minus; _ } as op :: rest -> 
      let right, tokens = unary rest state in
      Unary (op, right), tokens
  | _ -> primary tokens state

and primary tokens state =
  match tokens with
  | {kind=Number _; _} as t :: rest -> Literal t.kind, rest 
  | {kind=String _; _} as t :: rest -> Literal t.kind, rest 
  | {kind=True; _} as t :: rest -> Literal t.kind, rest 
  | {kind=False; _} as t :: rest -> Literal t.kind, rest 
  | {kind=Nil; _} as t :: rest -> Literal t.kind, rest 
  | {kind=Left_paren; _} :: rest -> 
      let expr, tokens = expression rest state in
      (match tokens with 
       | {kind=Right_paren; _} :: rest -> Grouping expr, rest
       | t :: _ -> parse_error t "Expect ')' after expression." state
       | [] -> parse_error {kind=EOF; lexeme=""; line=0} "Expect ')' after expression." state)
  | t :: _ -> parse_error t "Expect expression." state
  | [] -> raise ParseError

let parse tokens state =
  try 
    let expr, _ = expression tokens state in
    Some expr
  with 
  | ParseError -> None

