exception Wrong_input;;

let rec dfact : int -> int
= fun n -> match n with
           | a when a <= 0 -> raise Wrong_input
           | 1 -> 1
           | 2 -> 2
           | _ -> n * dfact (n - 2);;