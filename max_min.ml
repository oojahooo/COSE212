let rec max : int list -> int
= fun lst -> match lst with
             | [] -> -999
             | [hd] -> hd
             | hd::tl -> let m = max tl
                         in if hd < m then m
                               else hd;;

let rec min : int list -> int
= fun lst -> match lst with
             | [] -> 999
             | [hd] -> hd
             | hd::tl -> let m = min tl
                         in if hd > m then m
                               else hd;;