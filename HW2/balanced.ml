type mobile = branch * branch (* left and rigth branches *)
and branch = SimpleBranch of length * weight
		   | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weight_of_branch : branch -> int
= fun b ->
  match b with
  | SimpleBranch (l, w) -> w
  | CompoundBranch (l, (a,b)) -> (weight_of_branch a) + (weight_of_branch b)

let length_of_branch : branch -> int
= fun b ->
  match b with
  | SimpleBranch (l, w) -> l
  | CompoundBranch (l, w) -> l

let checkbalance : branch -> int
= fun b ->
  (weight_of_branch b) * (length_of_branch b)

let rec balanced : mobile -> bool
= fun m ->
  match m with
  | (l,r) ->
    let cc = (checkbalance l) = (checkbalance r) in
    cc &&
    (match l, r with
    | SimpleBranch (_,_), SimpleBranch (_,_) -> true
    | SimpleBranch (_,_), CompoundBranch (_,m) -> balanced m
    | CompoundBranch (_,m), SimpleBranch (_,_) -> balanced m
    | CompoundBranch (_,m1), CompoundBranch (_,m2) -> (balanced m1) && (balanced m2));;