(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List
open Ostap

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
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

      let binarylist operatorslist = 
      let listof operators = (ostap ($(operators)), 
      fun expr1 expr2 -> Binop (operators, expr1, expr2)) in 
      List.map listof operatorslist;;


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)

    ostap (
      primary: v:IDENT {Var v} | v:DECIMAL {Const v} | -"(" parse -")";
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
    (* loop with a post-condition       *) | Repeat of Expr.t * t  with show

                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)

    let rec eval sio stmt =
      let (statement, input, output) = sio in
      match stmt with
	|Read v -> 
	let head = hd input in 
	let tail = tl input in (Expr.update v head statement, tail, output)
	|Write expr ->  
	let result = Expr.eval statement expr in (statement, input, output @ [result])
	|Assign(v, expr) -> 
	let result = Expr.eval statement expr in (Expr.update v result statement, input, output)
	|Seq(seq1, seq2) -> (eval (eval (statement, input, output) seq1) seq2) 
	| Skip -> (statement, input, output)
        | If (expr, s1, s2) -> if Expr.eval statement expr != 0 then eval (statement, input, output) s1 else eval (statement, input, output) s2
        | While  (expr, s) -> if Expr.eval statement expr != 0 then eval (eval (statement, input, output) s) stmt else (statement, input, output)
        | Repeat (expr, s) -> 
        let (statement', input', output') = eval (statement, input, output) s in
        if Expr.eval statement' expr == 0 then eval (statement', input', output') stmt else (statement', input', output')
	
	let rec elif fi sf = match fi with
        | [] -> sf
        | (expr, s)::fi' -> If (expr, s, elif fi' sf)
	
	(* Statement parser *)
    ostap (
            parse:
       st:stmt ";" ps:parse {Seq (st, ps)}
      | stmt;
      stmt:
       "read" "(" v:IDENT ")"             {Read v}
      | "write" "(" expr:!(Expr.parse) ")" {Write expr}
      | v:IDENT ":=" expr:!(Expr.parse)    {Assign (v, expr)}            
      | %"skip" {Skip}
      | %"while" expr: !(Expr.parse) %"do" s:parse %"od" {While (expr, s)}
      | %"repeat" s:parse "until" expr:!(Expr.parse) {Repeat (expr, s)}
      | %"for" init:parse "," expr:!(Expr.parse) "," step:parse %"do" s:parse %"od" {Seq (init, While (expr, Seq (s, step)))}
      | %"if" expr: !(Expr.parse) 
        %"then" s:parse 
            s2:(%"elif" !(Expr.parse) %"then" parse)*
            sf:(%"else" parse)?
        %"fi" {If (expr, s, 
            match sf with 
              | None -> elif s2 Skip
              | Some expr -> elif s2 expr 
            )}
      
     )


  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
