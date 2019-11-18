let smallest_divisor : int -> int
= fun n -> 
    let rec is_divisor (d:int) =
      if d * d > n then n
      else (if n mod d = 0 then d
            else is_divisor (d+1))
    in is_divisor 2;;