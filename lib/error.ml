open Printf

type state = 
  { mutable had_err : bool; }

let report line where message state = 
  eprintf "[line %d] Error %s: %s\n" line where message;
  state.had_err <- true

let err line message state = 
  report line "" message state

