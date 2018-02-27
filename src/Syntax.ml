(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
open List
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval state expr = 
    match expr with
    | Var v -> state v 
    | Const c -> c 
    | Binop (operators, expr1, expr2) ->
    let right = eval state expr1 in
    let left = eval state expr2 in
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

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    

  let rec eval sio stmt =
      let (statement, input, output) = sio in
      match stmt with
	|Read v -> let head = hd input in let tail = tl input in (Expr.update v head statement, tail, output)
	|Write expr ->  let result = Expr.eval statement expr in (statement, input, output @ [result])
	|Assign(v, expr) -> let result = Expr.eval statement expr in (Expr.update v result statement, input, output)
	|Seq(seq1, seq2) -> (eval (eval (statement, input, output) seq1) seq2) 
end
(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o