open GT       
open Language
open List
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  

  let check_cond_jmp cond value = 
  match cond with
  | "nz" -> value <> 0
  | "z" -> value = 0 


   (*Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  


let rec eval env conf = function 
	| [] -> conf
	| inst::prog -> 
		match inst, conf with
		| BINOP operators, (cstack, left::right::stack_tail, stmt_conf) -> 
			let res = Expr.comp operators right left in
			eval env (cstack, res::stack_tail, stmt_conf) prog
		| CONST z, (cstack, stack, stmt_conf) ->
			eval env (cstack, z::stack, stmt_conf) prog
		| READ, (cstack, stack, (st, z::i, o)) ->
			eval env (cstack, z::stack, (st, i, o)) prog
		| WRITE, (cstack, z::stack, (st, i, o)) ->
			eval env (cstack, stack, (st, i, o @ [z])) prog
		| LD x, (cstack, stack, (st, i, o)) ->
			let res = State.eval st x 
			in 	eval env (cstack, res::stack, (st, i, o)) prog
		| ST x, (cstack, z::stack, (st, i, o)) ->
			let st' =	State.update x z st 
			in 	eval env (cstack, stack, (st', i, o)) prog
		| LABEL label, conf -> eval env conf prog
		| JMP label, conf -> eval env conf (env#labeled label)
		| CJMP (cond, label), (cstack, stack, stmt_conf) -> (
			eval env (cstack, List.tl stack, stmt_conf)
      (if (check_cond_jmp cond (List.hd stack)) then (env#labeled label) else prog)
		)
		| CALL name, (cstack, stack, (st, i, o)) ->
			eval env ((prog, st)::cstack, stack, (st, i, o)) (env#labeled name)
		| BEGIN (args, xs), (cstack, stack, (st, i, o)) ->
			let rec init_args state = function
				| a::args, z::stack -> 
					let state', stack' = init_args state (args, stack)
					in State.update a z state', stack'
				| [], stack -> state, stack
			in let s', stack' = init_args (State.enter st @@ args @ xs) (args, stack)
			in eval env (cstack, stack', (s', i, o)) prog
		| END, (cstack, stack, (st, i, o)) -> 
		(
			match cstack with
			| (p', st')::cstack' -> 
				eval env (cstack', stack, (State.leave st st', i, o)) p'
			| [] -> conf
		)	
(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

  
  let rec exprcompile expr =
  match expr with
  | Expr.Const c          -> [CONST c]
  | Expr.Var v            -> [LD v]
  | Expr.Binop (operators, expr1, expr2) -> (exprcompile expr1) @ (exprcompile expr2) @ [BINOP operators]
  | Expr.Call (name, param) ->
    List.concat (List.map exprcompile (List.rev param)) @ [CALL name]
  
    let label = object 
    val mutable id = 0
    method next = 
      id <- (id + 1);
      "L" ^ string_of_int id
  end
  
(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let compile (defs, stmt) = 
let rec compile' stmt =
  match stmt with
   |Stmt.Assign (v, expr) -> (exprcompile expr) @ [ST v]
   |Stmt.Read v        	 -> [READ] @ [ST v]
   |Stmt.Write expr       -> (exprcompile expr) @ [WRITE]
   |Stmt.Seq (seq1, seq2) -> compile' seq1 @ compile' seq2
   |Stmt.While (expr, seq) ->
    let llabel = label#next in
    let postlabel = label#next in
    let body = compile' seq in
    ([JMP llabel] @ [LABEL postlabel] @ body @ [LABEL llabel] @ exprcompile expr @ [CJMP ("nz", postlabel)])
   |Stmt.Repeat (expr, seq) ->
    let postlabel = label#next in
    let body = compile' seq in
    ([LABEL postlabel] @ body @ exprcompile expr @ [CJMP ("z", postlabel)])
   |Stmt.Skip -> []
   |Stmt.If (expr, s1, s2) ->
    let ella = label#next in
    let nextlabel = label#next in
    let curcase = compile' s1 in
    let lcase = compile' s2 in
    (exprcompile expr @ [CJMP ("z", ella)] @ curcase @ [JMP nextlabel] @ [LABEL ella] @ lcase @ [LABEL nextlabel])
   |Stmt.Call (name, param) -> 
    List.concat (List.map exprcompile param) @ [CALL name] 
   |Stmt.Return expr -> (
      match expr with
      | None -> [END]
      | Some expr -> exprcompile expr @ [END]
    )
    | _ -> failwith "Undefined Behavior"
    in
   let compile (name, (param, loc, body)) = 
      [LABEL name; BEGIN (param, loc)] @ compile' body @ [END] in
    compile' stmt @ [END] @ concat (map compile defs)


