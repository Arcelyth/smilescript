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

let run line state is_repl =
  let scanner = { Lexer.source=line; start=0; current=0; line_num=1} in
  let tokens = Lexer.scan_tokens scanner state in

  if is_repl then
    try
      let expr, rest = Parser.expression tokens in
      match rest with
      | [{kind=Smc.EOF; _}] | [] ->
          let v = Interpreter.evaluate_expr expr state in
          print_endline (Interpreter.string_of_value v)
      | _ -> raise Exit
    with _ ->
      try
        let stmts = Parser.parse tokens state in
        Resolver.resolve_stmts stmts state;
        Interpreter.interpret stmts state 
      with e -> eprintf "Error: %s\n" (Printexc.to_string e)
  else
    let stmts = Parser.parse tokens state in
    Resolver.resolve_stmts stmts state;
    Interpreter.interpret stmts state

let run_file path state = 
  let content = read_file path in
  run content state false; 
  if state.had_err = true then exit(65);
  if state.had_runtime_err = true then exit(70)

let rec run_prompt state = 
  print_string "> ";
  flush stdout;
  match (try Some (input_line stdin) with End_of_file -> None) with
  | None -> print_newline (); exit 0 
  | Some line ->
      (try run line state true with e -> eprintf "Error: %s\n" (Printexc.to_string e));
      state.had_err <- false;
      run_prompt state

let () = 
  let args = Sys.argv in
  let init_env = { Smc.values = Hashtbl.create 32; enclosing = None } in
  let state = {
    Smc.had_err = false;
    had_runtime_err = false;
    cur_env = init_env;
    globals = init_env;
    locals = Hashtbl.create 256;
    scopes = ref [];
  }
  in
  match Array.length args with 
  | 1 -> run_prompt state             
  | 2 -> run_file args.(1) state
  | _ ->                           
      printf "Usage: smc [script]\n";
      exit 64
