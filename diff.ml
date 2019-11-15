type aexp =
  Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp =
fun (exp, x) ->
  match exp with
    Const n -> Const 0
  | Var y -> if x = y then Const 1 else Const 0
  | Power (y, n) ->
      if n < 0 then raise (Failure "Invalid Input")
      else if n = 0 || x <> y then Const 0
      else Times [Const n; Power (y, n - 1)]
  | Times [hd] -> diff (hd, x)
  | Times (hd :: tl) ->
      Sum [Times (diff (hd, x) :: tl); Times [hd; diff (Times tl, x)]]
  | Sum [hd] -> diff (hd, x)
  | Sum (hd :: tl) -> Sum [diff (hd, x); diff (Sum tl, x)]
  | _ -> raise (Failure "Invalid Input")
;;