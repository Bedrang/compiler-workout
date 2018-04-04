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
    let emp x = failwith (Printf.sprintf "%s is undefined" x)
    let empty = {g = emp; l = emp; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = 
    let update' f y = if x = y then v else f y in 
    if mem x s.scope then { s with l = update' s.l } else { s with g = update' s.g }
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if mem x s.scope then s.l x else s.g x 

    (* Creates a new scope, based on a given state *)
    let enter st xs = {g = st.g; l = emp; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st with g= st'.g}

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
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let rec eval state expr = 
    match expr with
    | Var v -> State.eval state v 
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
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env sio stmt =
      let (statement, input, output) = sio in
      match stmt with
	|Read v -> 
	let head = hd input in 
	let tail = tl input in (State.update v head statement, tail, output)
	|Write expr ->  
	let result = Expr.eval statement expr in (statement, input, output @ [result])
	|Assign(v, expr) -> 
	let result = Expr.eval statement expr in (State.update v (result) statement, input, output)
	|Seq(seq1, seq2) -> (eval env (eval env (statement, input, output) seq1) seq2) 
	|Skip -> (statement, input, output)
        |If (expr, s1, s2) -> if Expr.eval statement expr != 0 then eval env (statement, input, output) s1 else eval env (statement, input, output) s2
        |While  (expr, s) -> if Expr.eval statement expr != 0 then eval env (eval env (statement, input, output) s) stmt else (statement, input, output)
        |Repeat (expr, s) -> 
        let (statement', input', output') = eval env (statement, input, output) s in
        if Expr.eval statement' expr == 0 then eval env (statement', input', output') stmt else (statement', input', output')
	|Call (p, expr) ->
	let param, loc, body = env#definition p in 
	let args = combine param (map (Expr.eval statement) expr) in
	let init = State.enter statement (param @ loc) in 
        let state = fold_left (fun statement (x, v) -> State.update x v statement) init args in
        let statement', input, output = eval env (state, input, output) body 
        in State.leave statement statement', input, output
                                
	let rec elif fi sf = match fi with
        | [] -> sf
        | (expr, s)::fi' -> If (expr, s, elif fi' sf)
	
                                
    (* Statement parser *)
      ostap (
      parse: seq | stmt;
      stmt: read | write | assign | if_ | while_ | for_ | repeat_ | skip | call;
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
      seq: left_st:stmt -";" right_st:parse { Seq (left_st, right_st) }
    )
      
  end


(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      argument: IDENT;
      parse:
        "fun" fname:IDENT "(" args: !(Util.list0 argument) ")"
        locals: (%"local" !(Util.list argument))?
        "{" body: !(Stmt.parse) "}" { (fname, (args, (match locals with None -> [] | Some l -> l), body))}
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
  let module DefMap = Map.Make (String) in
  let definitionsMap = List.fold_left (fun m ((name, _) as definitions) -> DefMap.add name definitions m) DefMap.empty defs in
  let env = (object method definition name = snd (DefMap.find name definitionsMap) end) in
  let _, _, output = Stmt.eval env (State.empty, i, []) body
  in output
                                   
(* Top-level parser *)
let parse = ostap (
  defs:!(Definition.parse) * body:!(Stmt.parse) {
    (defs, body) 
  }
)                                           
