open Printf

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
  | Break | Continue

  | EOF 


type token = {
  kind : token_type;
  lexeme : string;  
  line : int; 
}


type expr = 
  | Assign of token * expr
  | Binary of expr * token * expr
  | Logical of expr * token * expr
  | Unary of token * expr
  | Literal of token_type
  | Grouping of expr
  | Variable of token
  | Call of expr * token * expr list
  | Get of expr * token
  | Set of expr * token * expr 

type stmt =
  | PrintStmt of expr
  | ExprStmt of expr
  | VarStmt of token * expr
  | Block of stmt list
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt * stmt option
  | FuncStmt of token * token list * stmt list
  | ReturnStmt of token * expr
  (* need to ensure all the stmt are FuncStmt *)
  | Class of token * stmt list
  | BreakStmt
  | ContinueStmt

(* value type in runtime *)
type value = 
  | VNum of float
  | VStr of string
  | VBool of bool
  | VCallable of callable
  | VClass of smc_class
  | VInst of smc_instance
  | VNil

and func_type = 
  | TypeFunc
  | TypeNone
  | TypeMethod

and smc_class = {
  class_name : string;
  methods : (string, callable) Hashtbl.t;
}

and smc_instance = {
  klass : smc_class;
  fields : (string, value) Hashtbl.t;
}

and state = { 
  mutable had_err : bool; 
  mutable had_runtime_err : bool;
  mutable cur_env : env;
  globals : env;
  locals : (expr, int) Hashtbl.t;
  mutable cur_func : func_type;
  mutable scopes : (string, bool) Hashtbl.t list ref
}

and env = {
  values : (string, value) Hashtbl.t;
  enclosing : env option; 
}

and callable = {
  arity : int;
  call : state -> value list -> value;
}


let report line where message state = 
  printf "[line %d] Error %s: %s\n" line where message;
  state.had_err <- true

let err line message state = 
  report line "" message state


