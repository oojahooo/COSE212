type exp =
  X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec apply : exp -> int -> int =
fun e x ->
  match e with
    X -> x
  | INT n -> n
  | ADD (e1, e2) -> apply e1 x + apply e2 x
  | SUB (e1, e2) -> apply e1 x - apply e2 x
  | MUL (e1, e2) -> apply e1 x * apply e2 x
  | DIV (e1, e2) -> apply e1 x / apply e2 x
  | SIGMA (e1, e2, e3) ->
      let i = apply e1 x in
      let j = apply e2 x in
      if i > j then raise (Failure "Invalid Input")
      else if i = j then apply e3 i
      else apply e3 i + apply (SIGMA (ADD (e1, INT 1), e2, e3)) x
  
let rec calculator : exp -> int =
fun exp ->
  match exp with
    X -> raise (Failure "Invalid Input")
  | INT n -> n
  | ADD (e1, e2) -> calculator e1 + calculator e2
  | SUB (e1, e2) -> calculator e1 - calculator e2
  | MUL (e1, e2) -> calculator e1 * calculator e2
  | DIV (e1, e2) -> calculator e1 / calculator e2
  | SIGMA (e1, e2, e3) ->
      let i = calculator e1 in
      let j = calculator e2 in
      if i > j then raise (Failure "Invalid Input")
      else if i = j then apply e3 i
      else apply e3 i + calculator (SIGMA (ADD (e1, INT 1), e2, e3))
;;