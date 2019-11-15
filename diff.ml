type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
  | Const _ -> Const 0
  | Var v -> if v = x then Const 1 else Const 0
  | Power (v, n) -> if n = 0 then Const 0 else Times ((Const n)::[Power (v, (n-1))])
  | Times [] -> Const 0
  | Sum [] -> Const 0
  | Times (hd::tl) -> Sum ((Times (diff (hd, x)::tl))::[Times (hd::[diff (Times tl, x)])])
  | Sum (hd::tl) -> Sum (listdiff (hd::tl, x))

and listdiff : aexp list * string -> aexp list
= fun (explst, x) ->
  match explst with
  | [] -> []
  | hd::tl -> (diff (hd, x))::listdiff (tl, x);;