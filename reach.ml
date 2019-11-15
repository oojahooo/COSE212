type graph = (vertex * vertex) list
and vertex = int

let rec insert : 'a -> 'a list -> 'a list
= fun a l ->
  match l with
  | [] -> [a]
  | hd::tl -> 
    if hd < a then hd::(insert a tl)
    else if hd = a then hd::tl
         else a::hd::tl

let rec adj : vertex * vertex -> vertex list -> vertex list
= fun e vlist ->
  match vlist with
  | [] -> []
  | v::vs -> 
    match e with (x,y) ->
      if x = v then insert y vlist
      else insert v (adj e vs)

let rec e_closures : graph -> vertex list -> vertex list
= fun g vlist ->
  match g with
  | [] -> vlist
  | hd::tl ->
    let nsl = adj hd vlist
      in if vlist <> nsl then e_closures tl nsl
         else e_closures tl vlist

let rec untilfinish : graph -> vertex list -> vertex list
= fun g vlist ->
  let ec = e_closures g vlist
  in if e_closures g ec = ec then ec
     else untilfinish g ec

let rec reach : graph * vertex -> vertex list
= fun (g, v) -> 
  untilfinish g [v];;