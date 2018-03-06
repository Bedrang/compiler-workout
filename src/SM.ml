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
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval ssio prg =
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
    eval (result :: (tl (tl stackop)), (sio)) rest
    | CONST c -> eval (c :: stackop, (sio)) rest
    | READ -> eval (hd input :: stackop, (statement, tl input, output)) rest
    | WRITE -> eval (tl stackop, (statement, input, output @ [hd stackop])) rest
    | LD v -> eval (statement v :: stackop, (sio)) rest
    | ST v -> eval (tl stackop, (Expr.update v (hd stackop) statement, input, output)) rest
  

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

let rec exprcompile expr =
  match expr with
  | Expr.Const c          -> [CONST c]
  | Expr.Var v            -> [LD v]
  | Expr.Binop (operators, expr1, expr2) -> (exprcompile expr1) @ (exprcompile expr2) @ [BINOP operators]
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
