type exp = X
		 | INT of int
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  match exp with
  | X -> raise (Failure "X")
  | INT n -> n
  | ADD (e1,e2) -> binop e1 e2 (+)
  | SUB (e1,e2) -> binop e1 e2 (-)
  | MUL (e1,e2) -> binop e1 e2 ( * )
  | DIV (e1,e2) -> binop e1 e2 (/)
  | SIGMA (e1,e2,e3) -> sigma (calculator e1) (calculator e2) e3

and binop e1 e2 op =
  op (calculator e1) (calculator e2)

and nbinop n e1 e2 op =
  op (calcul_x e1 n) (calcul_x e2 n)

and calcul_x exp n =
  match exp with
  | X -> n
  | INT num -> num
  | ADD (e1,e2) -> nbinop n e1 e2 (+)
  | SUB (e1,e2) -> nbinop n e1 e2 (-)
  | MUL (e1,e2) -> nbinop n e1 e2 ( * )
  | DIV (e1,e2) -> nbinop n e1 e2 (/)
  | SIGMA (e1,e2,e3) -> sigma (calculator e1) (calculator e2) e3

and sigma : int -> int -> exp -> int
= fun n1 n2 exp ->
  let n = calcul_x exp n2 in
  if n1 = n2 then n
  else (sigma n1 (n2-1) exp) + n;;