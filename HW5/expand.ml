type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

let rec used : var -> exp -> bool
= fun x e ->
  match e with
  | CONST n -> false
  | VAR v -> v = x
  | ADD (e1, e2) -> (used x e1) || (used x e2)
  | SUB (e1, e2) -> (used x e1) || (used x e2)
  | MUL (e1, e2) -> (used x e1) || (used x e2)
  | DIV (e1, e2) -> (used x e1) || (used x e2)
  | ISZERO e' -> used x e'
  | READ -> false
  | IF (e1, e2, e3) -> (used x e1) || (used x e2) || (used x e3)
  | LET (v, e1, e2) -> if v = x then used x e1 else (used x e1) || (used x e2)
  | LETREC (v1, v2, e1, e2) ->
    if v1 = x then false else
    if v2 = x then used x e2 else
    (used x e1) || (used x e2) 
  | PROC (v, e') -> if v = x then false else used x e'
  | CALL (e1, e2) -> (used x e1) || (used x e2)

let rec substitute : var -> exp -> exp -> exp
= fun x e1 e2 ->
  match e2 with
  | CONST n -> e2
  | VAR v -> if v = x then e1 else e2
  | ADD (e1', e2') -> ADD (substitute x e1 e1', substitute x e1 e2')
  | SUB (e1', e2') -> SUB (substitute x e1 e1', substitute x e1 e2')
  | MUL (e1', e2') -> MUL (substitute x e1 e1', substitute x e1 e2')
  | DIV (e1', e2') -> DIV (substitute x e1 e1', substitute x e1 e2')
  | ISZERO e' -> ISZERO (substitute x e1 e')
  | READ -> e2
  | IF (e1', e2', e3') -> IF (substitute x e1 e1', substitute x e1 e2', substitute x e1 e3')
  | LET (v, e1', e2') -> if v = x then LET (v, substitute x e1 e1', e2') else LET (v, substitute x e1 e1', substitute x e1 e2')
  | LETREC (v1, v2, e1', e2') ->
    if v1 = x then e2 else
    if v2 = x then LETREC (v1, v2, e1', substitute x e1 e2') else
    LETREC (v1, v2, substitute x e1 e1', substitute x e1 e2')
  | PROC (v, e') -> if v = x then e2 else PROC (v, substitute x e1 e')
  | CALL (e1', e2') -> CALL (substitute x e1 e1', substitute x e1 e2')

let rec expand : exp -> exp 
= fun e ->
  match e with
  | LET (x, e1, e2) -> if used x e2 then substitute x e1 (expand e2) else LET (x, e1, expand e2)
  | ADD (e1, e2) -> ADD (expand e1, expand e2)
  | SUB (e1, e2) -> SUB (expand e1, expand e2)
  | MUL (e1, e2) -> MUL (expand e1, expand e2)
  | DIV (e1, e2) -> DIV (expand e1, expand e2)
  | ISZERO e' -> ISZERO (expand e')
  | IF (e1, e2, e3) -> IF (expand e1, expand e2, expand e3)
  | LETREC (f, x, e1, e2) -> LETREC (f, x, expand e1, expand e2)
  | PROC (x, e') -> PROC (x, expand e')
  | CALL (e1, e2) -> CALL (expand e1, expand e2)
  | _ -> e
;;
