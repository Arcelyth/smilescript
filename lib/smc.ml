open Printf

(* value type in runtime *)
type value = 
  | VNum of float
  | VStr of string
  | VBool of bool
  | VNil



type state = 
  { mutable had_err : bool; 
    mutable had_runtime_err : bool;
    mutable cur_env : env;
  }
and env = {
  values : (string, value) Hashtbl.t;
  enclosing : env option; 
}

let report line where message state = 
  printf "[line %d] Error %s: %s\n" line where message;
  state.had_err <- true

let err line message state = 
  report line "" message state

