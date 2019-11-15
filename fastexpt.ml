exception Wrong_input;;

let rec fastexpt : int -> int -> int
= fun b n -> match n with
             | a when a < 0 -> raise Wrong_input
             | 0 -> 1
             | _ -> if n mod 2 = 0 then let bb = fastexpt b (n/2) in bb * bb
                    else b * fastexpt b (n-1);;