type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * var * exp * env
and env = (var * value) list

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ List.fold_left (fun s x -> s ^ ", " ^ x) "" (List.map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
= fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR v -> lookup_env v env
  | ADD (e1, e2) -> binop_ari env e1 e2 (+)
  | SUB (e1, e2) -> binop_ari env e1 e2 (-)
  | MUL (e1, e2) -> binop_ari env e1 e2 ( * )
  | DIV (e1, e2) -> binop_ari env e1 e2 (/)
  | EQUAL (e1, e2) -> binop_bool env e1 e2 (=)
  | LESS (e1, e2) -> binop_bool env e1 e2 (<)
  | NOT e ->
    (match (eval e env) with
    | Bool b -> if b then Bool false else Bool true
    | _ -> raise (Failure ("error: NOT must take bool")))
  | NIL -> List []
  | CONS (e1, e2) ->
    (let v1 = eval e1 env in
    let v2 = eval e2 env in
      match v1, v2 with
      | n1, List n2 -> List (n1::n2)
      | _ -> raise (Failure "error: CONS must take lists"))
  | APPEND (e1, e2) ->
    (let v1 = eval e1 env in
    let v2 = eval e2 env in
      match v1, v2 with
      | List n1, List n2 -> List (n1@n2)
      | _ -> raise (Failure "error: APPEND must take lists"))
  | HEAD e ->
    (match (eval e env) with
    | List (hd::_) -> hd
    | _ -> raise (Failure "error: HEAD must take lists"))
  | TAIL e ->
    (match (eval e env) with
    | List (_::tl) -> List tl
    | _ -> raise (Failure "error: TAIL must take lists"))
  | ISNIL e ->
    (match (eval e env) with
    | List l -> if l = [] then Bool true else Bool false
    | _ -> raise (Failure "error: ISNIL must take lists"))
  | IF (e1, e2, e3) ->
    (match (eval e1 env) with
    | Bool b -> if b then (eval e2 env) else (eval e3 env)
    | _ -> raise (Failure "error: IF must take bools"))
  | LET (x,e1,e2) ->
    (let v1 = eval e1 env in
    let v = eval e2 (extend_env (x,v1) env) in v)
  | LETREC (f,x,e1,e2) ->
    eval e2 (extend_env (f,RecProcedure (f,x,e1,env)) env)
  | LETMREC ((f,x,e1),(g,y,e2),e3) ->
    eval e3 (extend_env (g,MRecProcedure (g,f,y,e2,env)) (extend_env (f,MRecProcedure (f,g,x,e1,env)) env))
  | PROC (x,e) -> Procedure (x,e,env)
  | CALL (e1,e2) ->
    (match eval e1 env with
    | Procedure (x,e,env1) -> eval e (extend_env (x,(eval e2 env)) env1)
    | RecProcedure (f,x,e,env1) -> eval e (extend_env (x,(eval e2 env)) (extend_env (f,RecProcedure (f,x,e,env1)) env1))
    | MRecProcedure (f,g,x,e,env1) -> eval e (extend_env (x,(eval e2 env)) (extend_env (g,lookup_env g env) (extend_env (f,MRecProcedure (f,g,x,e,env1)) env1)))
    | _ -> raise (Failure "error: CALL must take procs"))
  | SEQ (e1, e2) -> let _ = eval e1 env in () ; (eval e2 env)

and binop_ari env e1 e2 op =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    match v1, v2 with
    | Int n1, Int n2 -> Int (op n1 n2)
    | _ -> raise (Failure "error: binop must take integers")

and binop_bool env e1 e2 op =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    match v1, v2 with
    | Int n1, Int n2 -> Bool (op n1 n2)
    | _ -> raise (Failure "error: binop must take integers")

let runml : program -> value
=fun pgm -> eval pgm empty_env;;
