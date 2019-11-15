type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun f -> match f with
           | True -> true
           | False -> false
           | Not f1 -> not (eval f1)
           | AndAlso (f1, f2) -> (eval f1) && (eval f2)
           | OrElse (f1, f2) -> (eval f1) || (eval f2)
           | Imply (f1, f2) -> if eval f1 then (if eval f2 then true else false)
                               else true
           | Equal (e1, e2) -> let rec evalnum e =
                               match e with
                               | Num n -> n
                               | Plus (ex1, ex2) -> (evalnum ex1) + (evalnum ex2)
                               | Minus (ex1, ex2) -> (evalnum ex1) - (evalnum ex2)
                               in evalnum e1 = evalnum e2;;