open Lexer
open Printf
open Smc

type expr = 
  | Assign of token * expr
  | Binary of expr * token * expr
  | Logical of expr * token * expr
  | Unary of token * expr
  | Literal of token_type
  | Grouping of expr
  | Variable of token

type stmt =
  | PrintStmt of expr
  | ExprStmt of expr
  | VarStmt of string * expr
  | Block of stmt list
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt 

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
  {kind=If; _} :: rest -> if_statement rest
  | {kind=While; _} :: rest -> while_statement rest
  | {kind=Print; _} :: rest -> print_statement rest
  | {kind=Left_brace; _} :: rest -> 
      let stmts, tokens_after_brace = block rest in
      Block stmts, tokens_after_brace
  | tks -> expr_statement tks

and while_statement tokens = 
  let tokens2 = consume tokens Left_paren "Expect '(' after 'while'." in
  let cond, tokens3 = expression tokens2 in
  let tokens4 = consume tokens3 Right_paren "Expect ')' after 'while'." in
  let body, tokens5 = statement tokens4 in
  WhileStmt (cond, body), tokens5

and block tokens =
  let rec loop acc tks = 
    match tks with 
    | {kind=Right_brace; _} :: rest -> 
        (List.rev acc, rest)
    | {kind=EOF; _} :: _ | [] -> 
        raise (ParseError (List.hd tks, "Expect '}' after block."))
    | _ -> 
        let stmt, next_tks = declaration tks in
        loop (stmt :: acc) next_tks
  in
  loop [] tokens

and if_statement tokens = 
  let tokens2 = consume tokens Left_paren "Expect '(' after 'if'." in
  let cond, rest = expression tokens2 in
  let rest2 = consume rest Right_paren "Expect ')' after if condition." in
  let then_br, rest3 = statement rest2 in
  match rest3 with 
  | {kind=Else; _} :: rest4 -> 
      let else_br, rest5 = statement rest4 in
      IfStmt (cond, then_br, else_br), rest5
  | _ -> IfStmt (cond, then_br, ExprStmt (Literal Nil) ), rest3
  
and print_statement tokens =
  let e, tokens = expression tokens in 
  let tokens = consume tokens Semicolon "Expect ';' after value." in
  PrintStmt e, tokens

and expr_statement tokens = 
  let e, tokens = expression tokens in 
  let tokens = consume tokens Semicolon "Expect ';' after expression." in
  ExprStmt e, tokens

and expression tokens = 
  assignment tokens 

and assignment tokens = 
  let e, tokens = logic_or tokens in 
  match tokens with 
  | {kind=Equal; _} as op :: tokens2 -> 
      let v, rest = assignment tokens2 in 
      (match e with 
       | Variable t -> 
           Assign (t, v), rest
       | _ -> 
           raise (ParseError (op, "Invalid assignment target.")))
  | _ -> 
      e, tokens

and logic_or tokens = 
  let e, tokens = logic_and tokens in
  let rec loop e = function 
    | {kind=Or; _} as op :: rest -> 
        let right, rest2 = logic_and rest in
        loop (Logical (e, op, right)) rest2
    | rest -> e, rest
  in
  loop e tokens
    
and logic_and tokens = 
  let e, tokens = equality tokens in
  let rec loop e = function 
    | {kind=And; _} as op :: rest -> 
        let right, rest2 = equality rest in
        loop (Logical (e, op, right)) rest2
    | rest -> e, rest
  in
  loop e tokens

and equality tokens =
  let expr, tokens = comparison tokens  in
  let rec loop expr = function
    | {kind=Bang_equal | Equal_equal; _ } as op :: rest -> 
        let right, tokens = comparison rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in
  loop expr tokens

and comparison tokens =
  let expr, tokens = term tokens  in
  let rec loop expr = function 
    | {kind=Greater_equal | Greater | Less_equal | Less; _ } as op :: rest -> 
        let right, tokens = term rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and term tokens =
  let expr, tokens = factor tokens  in
  let rec loop expr = function 
    | {kind=Minus | Plus; _ } as op :: rest -> 
        let right, tokens = factor rest  in 
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and factor tokens =
  let expr, tokens = unary tokens  in
  let rec loop expr = function 
    | {kind=Slash | Star; _ } as op :: rest -> 
        let right, tokens = unary rest  in
        loop (Binary (expr, op, right)) tokens
    | rest -> expr, rest
  in 
  loop expr tokens

and unary tokens =
  match tokens with
  | {kind=Bang | Minus; _ } as op :: rest -> 
      let right, tokens = unary rest  in
      Unary (op, right), tokens
  | _ -> primary tokens 

and primary tokens =
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
  | Assign (t, v) -> 
      sprintf "(assign %s %s)" t.lexeme (print_expr v)
  | Logical (left, op, right) -> 
      parenthesize op.lexeme [left; right]


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
