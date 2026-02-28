open Smc

let locals : (expr, int) Hashtbl.t = Hashtbl.create 256

let begin_scope scopes =
  scopes := Hashtbl.create 8 :: !scopes

let end_scope scopes =
  scopes := List.tl !scopes

let declare tk state =
  match !(state.scopes) with
  | [] -> ()
  | top :: _ ->
      if Hashtbl.mem top tk.lexeme then
        err tk.line ("Already a variable with name " ^ tk.lexeme ^ " in this scope.") state
      else
        Hashtbl.add top tk.lexeme false

let define name scopes =
  match !scopes with
  | [] -> ()
  | top :: _ ->
      Hashtbl.replace top name.lexeme true

let rec resolve_stmt state stmt = 
  match stmt with 
  | Block stmts -> 
      begin_scope state.scopes; 
      resolve_stmts stmts state; 
      end_scope state.scopes
  | VarStmt (ident, init_expr) -> 
      declare ident state; 
      (match init_expr with 
      | Literal Nil -> ()
      | _ -> resolve_expr state init_expr);
      define ident state.scopes
  | FuncStmt (name, params, body) -> 
      declare name state; 
      define name state.scopes;
      let enclosing_func = state.cur_func in
      state.cur_func <- TypeFunc;
      begin_scope state.scopes;
      List.iter (fun x -> declare x state; define x state.scopes) params;
      resolve_stmts body state;
      end_scope state.scopes;
      state.cur_func <- enclosing_func
  | ExprStmt e -> 
      resolve_expr state e 
  | IfStmt (cond, then_b, else_b) ->  
      resolve_expr state cond; 
      resolve_stmt state then_b;
      (match else_b with 
      | ExprStmt (Literal Nil) -> ()
      | _ -> resolve_stmt state else_b)
  | PrintStmt e -> 
    resolve_expr state e
  | ReturnStmt (keyword, e) -> 
      if state.cur_func = TypeNone then
        err keyword.line "Can't return from top-level code." state;
      (match e with
      | Literal Nil -> ()
      | _ -> resolve_expr state e)
  | WhileStmt (e, body, incr) -> 
      resolve_expr state e;
      resolve_stmt state body;
      (match incr with 
      | Some e2 -> resolve_stmt state e2
      | None -> ())
  | Class (name, methods) -> 
      declare name state;
      define name state.scopes;
      List.iter (fun m -> 
        match m with
        | FuncStmt (_, params, body) ->
            let enclosing_func = state.cur_func in
            state.cur_func <- TypeMethod; 
            begin_scope state.scopes;
            List.iter (fun p -> declare p state; define p state.scopes) params;
            resolve_stmts body state;
            end_scope state.scopes;
            state.cur_func <- enclosing_func
        | _ -> ()
      ) methods
  | _ -> ()

and resolve_stmts stmts state = 
  List.iter (resolve_stmt state) stmts

and resolve_local state expr name_token =
  let rec search_scopes depth = function
    | [] -> () 
    | head :: rest ->
        if Hashtbl.mem head name_token.lexeme then
          Hashtbl.add state.locals expr depth
        else
          search_scopes (depth + 1) rest
  in
  search_scopes 0 !(state.scopes)

and resolve_expr state expr =
  match expr with
  | Variable name_token ->
      (match !(state.scopes) with
      | head :: _ ->
          (match Hashtbl.find_opt head name_token.lexeme with
          | Some false -> 
              err name_token.line "Can't read local variable in its own initializer." state
          | _ -> ())
      | _ -> ());
      resolve_local state expr name_token
  | Assign (name_token, value_expr) ->
      resolve_expr state value_expr;
      resolve_local state expr name_token

  | Binary (left, _, right) ->
      resolve_expr state left;
      resolve_expr state right
  | Grouping e -> resolve_expr state e
  | Call (callee, _, args) ->
      resolve_expr state callee;
      List.iter (resolve_expr state) args
  | Get (e, _) -> 
      resolve_expr state e
  | Set (e, _, v) -> 
      resolve_expr state e;
      resolve_expr state v;
  | Logical (l, _ , r) -> 
      resolve_expr state l;
      resolve_expr state r
  | Unary (_, e) -> 
      resolve_expr state e
  | _ -> ()







