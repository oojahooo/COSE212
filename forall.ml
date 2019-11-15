let rec forall : ('a -> bool) -> 'a list -> bool
= fun f lst -> 
    match lst with
    | [] -> true
    | hd::tl -> (f hd) && (forall f tl);;