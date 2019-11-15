type btree =
  | Empty
  | Node of (int * btree * btree)
;;

let rec mem : int -> btree -> bool
= fun n t -> match t with
             | Empty -> false
             | Node (x,a,b) -> if x = n then true
                               else (mem n a) || (mem n b);;