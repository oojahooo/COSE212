exception Wrong_input;;

let rec pascal : int * int -> int
= fun (n, m) -> if n >= 0 then match m with
                               | 0 -> 1
                               | a when a = n -> 1
                               | _ -> if m < 0 || m > n then raise Wrong_input
                                      else pascal (n-1,m-1) + pascal (n-1,m)
                else raise Wrong_input