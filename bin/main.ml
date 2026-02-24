open Printf
open Smc_lib

let read_file path = 
  try
    let ic = open_in path in
    let buf = Buffer.create 4096 in
    try
      while true do
        Buffer.add_string buf (input_line ic);
        Buffer.add_char buf '\n'
      done;
      assert false
    with End_of_file ->
      close_in ic;
      Buffer.contents buf
  with Sys_error err ->
    eprintf "Error: %s\n" err;
    exit 64 


let run line state = 
  let scanner = { Lexer.source=line; start=0; current=0; line=1} in
  let tokens = Lexer.scan_tokens scanner state in
  List.iter Lexer.print_token tokens

let run_file path state = 
  let content = read_file path in
  run content state

let rec run_prompt state = 
  print_string "> ";
  flush stdout;
  match (try Some (input_line stdin) with End_of_file -> None) with
  | None -> print_newline (); exit 0 
  | Some line ->
      (try run line state with e -> eprintf "Error: %s\n" (Printexc.to_string e));
      state.had_err <- false;
      run_prompt state

let () = 
  let args = Sys.argv in
  let state = { Error.had_err = false } in
  match Array.length args with 
  | 1 -> run_prompt state             
  | 2 -> run_file args.(1) state
  | _ ->                           
      printf "Usage: smc [script]\n";
      exit 64
