(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
open List                         

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)


    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option

    
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *)                                                       
    
    let comp operators right left =
    let numericbool numbers = if numbers != 0 then true else false in 
    let boolnumeric numbers = if numbers then 1 else 0 in
    match operators with
    |"+" -> (right + left)
    |"-" -> (right - left)
    |"*" -> (right * left)
    |"/" -> (right / left)
    |"%" -> (right mod left)
    |"<"  -> boolnumeric (right < left)
    |"<=" -> boolnumeric (right <= left)
    |">"  -> boolnumeric (right > left)
    |">=" -> boolnumeric (right >= left)
    |"==" -> boolnumeric (right == left)
    |"!=" -> boolnumeric (right != left)
    |"!!" -> boolnumeric (numericbool right || numericbool left)
    |"&&" -> boolnumeric (numericbool right && numericbool left)
    | _ -> failwith "Error"

      let binarylist operatorslist = 
      let listof operators = (ostap ($(operators)), 
      fun expr1 expr2 -> Binop (operators, expr1, expr2)) in 
      List.map listof operatorslist;;  
    
    let rec eval env ((statement, input, output, r) as sior) expr = 
    match expr with
    | Var v -> let result = State.eval statement v in	(statement, input, output, Some result)
    | Const c -> (statement, input, output, Some c)
    | Binop (operators, expr1, expr2) ->
     let (statement', input', output', Some l) as sior' = eval env sior expr1 in
     let (statemen', input', output', Some r) = eval env sior' expr2 in
     let numbers = comp operators l r in 
     (statement', input', output', Some numbers)
    | Call (name, param) -> 
          let eval_arg arg_expr (sior, param) = 
            let ((_,  _,  _, Some r) as sior') = eval env sior arg_expr in
            (sior', (r :: param)) in
         let (sior', evaluated_args) = fold_right eval_arg param (sior, []) in
          env#definition env name evaluated_args sior' 
   


    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *)                                                       
    let rec eval env ((st, i, o, r) as conf) expr = failwith "Not implemented"
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)

     ostap (
      primary: p:IDENT "(" expr:!(Util.list0)[parse] ")" {Call (p, expr)} 
      | v:IDENT {Var v} 
      | v:DECIMAL {Const v} 
      | -"(" parse -")";
      parse: 
        !(Ostap.Util.expr
          (fun v -> v)
          [|
            `Lefta, binarylist ["!!"];
            `Lefta, binarylist ["&&"];
            `Nona,  binarylist [">="; ">"; "<="; "<"; "=="; "!="];
            `Lefta, binarylist ["+"; "-"];
            `Lefta, binarylist ["*"; "/"; "%"]
          |]
          primary
        )

    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
let rec eval env sior k stmt =
    	let sk stmt k =
    		if k = Skip then stmt
    		else Seq (stmt, k) in
      match stmt, sior with
    	| Assign (x, e), (statement, input, output, r) -> 
    		let (statement', input', output', Some value) = Expr.eval env sior e in
        eval env (State.update x value statement', input', output', r) Skip k
    	| Read x, (statement, z::input, output, r) ->
        eval env (State.update x z statement, input, output, r) Skip k
    	| Write e, (statement, input, output, r) ->
    		let (statement', input', output', Some value) = Expr.eval env sior e in
        eval env (statement', input', output' @ [value], r) Skip k
    	| Seq (a, b), sior ->
        eval env sior (sk b k) a        
    	| Skip, sior -> 
    		if k = Skip then sior
        else eval env sior Skip k       
    	| If (cond, the, els), (statement, input, output, r) ->
    		let (statement', input', output', Some value) as sior' = Expr.eval env sior cond in
    		let if_answer value = if value == 0 then els else the in
        eval env sior' k (if_answer value)        
    	| While (cond, body), sior ->
    		let (statement', input', output', Some res_cond) as sior' = Expr.eval env sior cond in
        if res_cond == 0 
        then eval env sior' Skip k
        else eval env sior' (sk stmt k) body        
    	| Repeat (cond, body), sior ->
        eval env sior (sk ( While(Expr.Binop("==", cond, Expr.Const 0), body)) k) body        
    	| Call (fun_name, args), (statement, input, output, r) ->
        let rec eval_args env sior = function
          | expr::args' ->
            let (statement', input', output', Some evaled_arg) = Expr.eval env sior expr in
            let evaled_args', sior' = eval_args env (statement', input', output', Some evaled_arg) args' in 
            evaled_arg::evaled_args', sior'
          | [] -> [], sior  
        in
        let evaled_args, sior' = eval_args env sior args in
        let sior'' = env#definition env fun_name evaled_args sior' in
        eval env sior'' Skip k       
      | Return expr, (statement, input, output, r) -> 
      (
        match expr with
        | None -> (statement, input, output, None)
        | Some expr -> Expr.eval env sior expr
      )

    let rec parse_elif_acts elif_acts parsed_else_act = 
      match elif_acts with
      [] -> parsed_else_act
      | (cond, act)::tail -> If (cond, act, parse_elif_acts tail parsed_else_act)

    let parse_elif_else elif_acts else_act = 
      let parsed_else_act = 
        match else_act with
        | None -> Skip
        | Some act -> act
      in parse_elif_acts elif_acts parsed_else_act

         
    (* Statement parser *)
    ostap (
      parse: seq | stmt;
      stmt: read | write | assign | if_ | while_ | for_ | repeat_ | skip | call | rtn;
      read: "read" "(" x:IDENT ")" { Read x };
      write: "write" "(" e:!(Expr.parse) ")" { Write e };
      assign: x:IDENT ":=" e:!(Expr.parse) { Assign (x, e) };
      if_: "if" e:!(Expr.parse) "then" s:parse "fi" {If (e, s, Skip)} 
         | "if" e:!(Expr.parse) "then" s1:parse else_elif:else_or_elif "fi" {If (e, s1, else_elif)};
      else_or_elif: else_ | elif_;
      else_: "else" s:parse {s};
      elif_: "elif" e:!(Expr.parse) "then" s1:parse s2:else_or_elif {If (e, s1, s2)};
      while_: "while" e:!(Expr.parse) "do" s:parse "od" {While (e, s)};
      for_: "for" init:parse "," e:!(Expr.parse) "," s1:parse "do" s2:parse "od" {Seq (init, While (e, Seq(s2, s1)))};
      repeat_: "repeat" s:parse "until" e:!(Expr.parse) {Repeat (e, s)};
      skip: "skip" {Skip};
      call: x:IDENT "(" args:!(Util.list0)[Expr.parse] ")" {Call (x, args)};
      seq: left_st:stmt -";" right_st:parse { Seq (left_st, right_st) };
      rtn: "return" e:(!(Expr.parse))? {Return e}

      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =
           let xs, locs, s      = snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
