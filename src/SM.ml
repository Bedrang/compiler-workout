open GT    
open List
open Language
       
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
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         

let rec eval env ssio prg =
    match prg with
    | [] -> ssio
    | insn :: rest ->
    let (stackop, sio) = ssio in
    let (statement, input, output) = sio in
    match insn with
    | BINOP operators ->
    let second = hd stackop in
    let first = hd (tl stackop) in
    let result = Expr.eval statement (Expr.Binop(operators, Expr.Const first, Expr.Const second)) in
    eval env (result :: (tl (tl stackop)), (sio)) rest
    | CONST c -> eval env (c :: stackop, (sio)) rest
    | READ -> eval env (hd input :: stackop, (statement, tl input, output)) rest
    | WRITE -> eval env (tl stackop, (statement, input, output @ [hd stackop])) rest
    | LD v -> eval env (statement v :: stackop, (sio)) rest
    | ST v -> eval env (tl stackop, (Expr.update v (hd stackop) statement, input, output)) rest
    | LABEL _ -> eval env ssio rest
    | JMP label -> eval env ssio (env#labeled label)
    | CJMP (cond, label)  -> 
      let (s::hd) = stackop in
      let x = match cond with
      | "nz" -> s <> 0
      | "z" -> s = 0 
      in eval env (hd, sio) (if (x) then (env#labeled label) else rest)


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
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

  let rec exprcompile expr =
  match expr with
  | Expr.Const c          -> [CONST c]
  | Expr.Var v            -> [LD v]
  | Expr.Binop (operators, expr1, expr2) -> (exprcompile expr1) @ (exprcompile expr2) @ [BINOP operators]
  
    let env = object 
    val mutable id = 0
    method next_label = 
      id <- (id + 1);
      "L" ^ string_of_int id
  end
(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

let rec compile stmt =
  match stmt with
  | Stmt.Assign (v, expr) -> (exprcompile expr) @ [ST v]
  | Stmt.Read v        	 -> [READ] @ [ST v]
  | Stmt.Write expr       -> (exprcompile expr) @ [WRITE]
  | Stmt.Seq (seq1, seq2) -> compile seq1 @ compile seq2
  | Stmt.While (expr, seq) ->
    let end_label = env#next_label in
    let loop_label = env#next_label in
    let body = compile seq in
    ([JMP end_label] @ [LABEL loop_label] @ body @ [LABEL end_label] @ exprcompile expr @ [CJMP ("nz", loop_label)])
  | Stmt.Repeat (expr, seq) ->
    let loop_label = env#next_label in
    let body = compile seq in
    ([LABEL loop_label] @ body @ exprcompile expr @ [CJMP ("z", loop_label)])
  | Stmt.Skip -> []
  | Stmt.If (expr, seq1, seq2) ->
    let else_label = env#next_label in
    let end_label = env#next_label in
    let current_case = compile seq1 in
    let last_case = compile seq2 in
    (exprcompile expr @ [CJMP ("z", else_label)] @ current_case @ [JMP end_label] @ [LABEL else_label] @ last_case @ [LABEL end_label])

