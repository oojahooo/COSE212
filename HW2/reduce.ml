let rec reduce : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
= fun f xs ys c -> 
    match xs, ys with
    | [], [] -> c
    | xhd::xtl, yhd::ytl -> reduce f xtl ytl (f xhd yhd c)
    | _, _ -> raise (Failure "X");;