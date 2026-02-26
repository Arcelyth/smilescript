open Printf
open Smc

exception SyntaxError of string

type token_type = 
  (* single-character tokens *)
  | Left_paren   (* ( *)
  | Right_paren  (* ) *)
  | Left_brace   (* { *)
  | Right_brace  (* } *)
  | Comma        (* , *)
  | Dot          (* . *)
  | Minus        (* - *)
  | Plus         (* + *)
  | Semicolon    (* ; *)
  | Slash        (* / *)
  | Star         (* * *)

  (* one or two character tokens *)
  | Bang         (* ! *)
  | Bang_equal   (* != *)
  | Equal        (* = *)
  | Equal_equal  (* == *)
  | Greater      (* > *)
  | Greater_equal(* >= *)
  | Less         (* < *)
  | Less_equal   (* <= *)

  (* literals *)
  | Identifier of string 
  | String of string   
  | Number of float       
  | Boolean of bool

  (* keywords *)
  | And | Class | Else | False | Fun | For | If | Nil | Or
  | Print | Return | Super | This | True | Var | While

  | EOF 


type token = {
  kind : token_type;
  lexeme : string;  
  line : int; 
}

let string_of_token_type = function
  | Left_paren -> "LEFT_PAREN"
  | Right_paren -> "RIGHT_PAREN"
  | Left_brace -> "LEFT_BRACE"
  | Right_brace -> "RIGHT_BRACE"
  | Comma -> "COMMA"
  | Dot -> "DOT"
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Semicolon -> "SEMICOLON"
  | Slash -> "SLASH"
  | Star -> "STAR"
  
  (* one or two character tokens *)
  | Bang -> "BANG"
  | Bang_equal -> "BANG_EQUAL"
  | Equal -> "EQUAL"
  | Equal_equal -> "EQUAL_EQUAL"
  | Greater -> "GREATER"
  | Greater_equal -> "GREATER_EQUAL"
  | Less -> "LESS"
  | Less_equal -> "LESS_EQUAL"
  
  | Identifier _ -> "IDENTIFIER"
  | String _ -> "STRING"
  | Number _ -> "NUMBER"
  | Boolean _ -> "BOOLEAN"
  
  | And -> "AND"
  | Class -> "CLASS"
  | Else -> "ELSE"
  | False -> "FALSE"
  | Fun -> "FUN"
  | For -> "FOR"
  | If -> "IF"
  | Nil -> "NIL"
  | Or -> "OR"
  | Print -> "PRINT"
  | Return -> "RETURN"
  | Super -> "SUPER"
  | This -> "THIS"
  | True -> "TRUE"
  | Var -> "VAR"
  | While -> "WHILE"
  
  | EOF -> "EOF"

let string_of_token token = 
  match token.lexeme with
  | "" -> (string_of_token_type token.kind)
  | l -> (string_of_token_type token.kind) ^ "," ^ l

let print_token token = printf "<%s>\n" (string_of_token token)

type scanner = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable line_num : int;
}

let scan_tokens sc state =
  let is_at_end () = sc.current >= String.length sc.source in

  let peek () = if is_at_end () then '\000' else sc.source.[sc.current] in

  let peek_next () = 
    if 
      sc.current + 1 >= String.length sc.source 
    then '\000' 
    else sc.source.[sc.current + 1]
  in
  let advance () =
    let c = sc.source.[sc.current] in
    sc.current <- sc.current + 1;
    c
  in

  let expected c =
    if is_at_end () then false
    else if sc.source.[sc.current] <> c then false
    else (sc.current <- sc.current + 1; true)
  in

  let new_token ty =
    let text = String.sub sc.source sc.start (sc.current - sc.start) in
    Some {kind = ty; lexeme = text; line = sc.line_num}
  in

  let rec eat_comment () =
    if peek () <> '\n' && not (is_at_end ()) then
      (ignore (advance ()); eat_comment ())
    else None 
  in

  let rec scan_string () =
    if is_at_end () then 
      (err sc.line_num "Unterminated string" state; None)
    else match advance () with
      | '"' -> 
          let value = String.sub sc.source (sc.start + 1) (sc.current - sc.start - 2) in
          new_token @@ String value
      | '\n' -> 
          sc.line_num <- sc.line_num + 1; scan_string ()
      | _ -> scan_string ()
  in

  let is_digit c = 
    let code = Char.code c in 
    code >= Char.code('0') && code <= Char.code('9')
  in

  let is_alpha = function 
    'A'..'Z' | 'a'..'z' | '_' -> true
    | _ -> false
  in

  let is_alpha_num c = is_alpha c || is_digit c in

  let scan_number () = 
    let rec loop_num () = 
      if is_digit (peek ()) then (
        ignore @@ advance ();
        loop_num ()
      )
    in 
    loop_num ();
    let num_str = String.sub sc.source sc.start (sc.current - sc.start) in
    if peek () = '.' && is_digit (peek_next ()) then (
      ignore @@ advance ();
      loop_num ();
      let num_str = String.sub sc.source sc.start (sc.current - sc.start) in
      new_token (Number (float_of_string num_str))
    ) else
      new_token (Number (float_of_string num_str)) 
   in
  let rec scan_ident () = 
    match is_alpha_num @@ peek () with 
    | true -> 
        ignore @@ advance ();
        scan_ident ()
    | false -> 
        match String.sub sc.source (sc.start) (sc.current - sc.start) with
        | "and" -> new_token And
        | "class" -> new_token Class
        | "else" -> new_token Else
        | "false" -> new_token @@ Boolean false 
        | "for" -> new_token For
        | "fun" -> new_token Fun
        | "if" -> new_token If
        | "nil" -> new_token Nil
        | "or" -> new_token Or
        | "print" -> new_token Print
        | "return" -> new_token Return
        | "super" -> new_token Super
        | "this" -> new_token This
        | "true" -> new_token @@ Boolean true
        | "var" -> new_token Var
        | "while" -> new_token While
        | text -> new_token @@ Identifier text
  in 
      
  let scan_token () = 
    match advance () with 
    | '(' -> new_token Left_paren
    | ')' -> new_token Right_paren
    | '{' -> new_token Left_brace
    | '}' -> new_token Right_brace
    | ',' -> new_token Comma
    | '.' -> new_token Dot
    | '-' -> new_token Minus
    | '+' -> new_token Plus
    | ';' -> new_token Semicolon
    | '*' -> new_token Star
    | '!' -> if expected '=' then new_token Bang_equal else new_token Bang
    | '=' -> if expected '=' then new_token Equal_equal else new_token Equal
    | '<' -> if expected '=' then new_token Less_equal else new_token Less
    | '>' -> if expected '=' then new_token Greater_equal else new_token Greater
    | '/' -> if expected '/' then eat_comment () else new_token Slash
    | ' ' | '\r' | '\t' -> None
    | '\n' -> sc.line_num <- sc.line_num + 1; None
    | '"' -> scan_string () 
    | c when is_digit c -> scan_number () 
    | c when is_alpha c -> scan_ident ()
    | _ -> err sc.line_num "Unexpected character" state; None
  in

  let rec loop acc =
    if is_at_end () then
      List.rev ({kind=EOF; lexeme=""; line=sc.line_num} :: acc)
    else begin
      sc.start <- sc.current;
      match scan_token () with
      | Some tok -> loop (tok :: acc)
      | None -> loop acc
    end
  in
  loop []
