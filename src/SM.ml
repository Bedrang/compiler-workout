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
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         

let rec eval env ssio prg =
    match prg with
    | [] -> ssio
    | insn :: rest ->
    let (cstack ,stackop, sio) = ssio in
    let (statement, input, output) = sio in
    match insn with
    | BINOP operators ->
    let second = hd stackop in
    let first = hd (tl stackop) in
    let result = Expr.eval statement (Expr.Binop(operators, Expr.Const first, Expr.Const second)) in
    eval env (cstack,result :: (tl (tl stackop)), (sio)) rest
    | CONST c -> eval env (cstack,c :: stackop, (sio)) rest
    | READ -> eval env (cstack, hd input :: stackop, (statement, tl input, output)) rest
    | WRITE -> eval env (cstack, tl stackop, (statement, input, output @ [hd stackop])) rest
    | LD v -> eval env (cstack, State.eval statement v :: stackop, (sio)) rest
    | ST v -> eval env (cstack, tl stackop, (State.update v (hd stackop) statement, input, output)) rest
    | LABEL _ -> eval env ssio rest
    | JMP label -> eval env ssio (env#labeled label)
    | CJMP (cond, label)  -> 
      let (s::hd) = stackop in
      let x = match cond with
      | "nz" -> s <> 0
      | "z" -> s = 0 
      in eval env (cstack, hd, sio) (if (x) then (env#labeled label) else rest)
    | CALL f -> eval env ((rest, statement)::cstack, stackop, sio) @@ env#labeled f
    | BEGIN (args, xs) ->
      let rec get_args statement = function
        | a::args, z::stackop -> let statement', stackop' = get_args statement (args, stackop)
        in State.update a z statement', stackop'
        | [], stackop -> statement, stackop
      in let statement', stackop' = get_args (State.enter statement @@ args @ xs) (args, stackop)
      in eval env (cstack, stackop', (statement', input, output)) rest
    | END ->
      match cstack with
      | (prg, s)::cstack' ->
        eval env (cstack', stackop, (State.leave s statement, input, output)) prg
      | [] -> ssio


(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
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
    List.concat (List.map exprcompile param) @ [CALL name] in
    let compile_def (name, (param, loc, body)) = [LABEL name; BEGIN (param, loc)] @ compile' body @ [END] in
    compile' stmt @ [END] @ List.concat (List.map compile_def defs)


