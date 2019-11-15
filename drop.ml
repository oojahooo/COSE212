let rec drop : ('a -> bool) -> 'a list -> 'a list
= fun f lst -> match lst with
               | [] -> []
               | hd::tl -> if f hd then drop f tl
                           else lst;;