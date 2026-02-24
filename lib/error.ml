open Printf

type state = 
  { mutable had_err : bool; }

let report line where message = 
  eprintf "[line %d] Error %s: %s\n" line where message

let err line message state = 
  report line "" message;
  state.had_err <- true

