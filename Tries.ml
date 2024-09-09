let children_from_char m c =
  List.assoc_opt c m

let update_children m c t =
  let rec f = function
    | (false, (ch, tr), [], r) -> 
        if c = ch then List.rev ((c, t)::r)
        else List.rev ((c, t)::((ch, tr)::r))
    | (true, (ch, tr), [], r) -> List.rev ((ch, tr)::r)
    | (false, (ch, tr), head::tail, r) -> 
        f (c = ch, head, tail, (ch, if c = ch then t else tr)::r)
    | (true, (ch, tr), head::tail, r) -> 
        f (true, head, tail, (ch, tr)::r)
  in
  match m with
  | [] -> [(c, t)]
  | h::t -> f (false, h, t, [])
      
let rec lookup (Trie (value, xs)) w =
  let n = String.length w in
  if n = 0 then value
  else try
      let h = String.get w 0 in
      let (_, (Trie (value, _) as t)) = List.find (fun (c, _) -> c = h) xs in
      if n = 1 then value
      else lookup t @@ String.sub w 1 (n - 1)
    with Not_found -> None

let rec insert (Trie (value, xs)) w v =
  let n = String.length w in
  if n = 0 then Trie (Some v, xs)
  else
    let h = String.get w 0 in
    let rec loop acc = function
      | (c, t) as y :: ys ->
          let t' = insert t (String.sub w 1 (n - 1)) v in
          if c = h then (List.rev @@ (c, t') :: acc) @ ys
          else loop (y :: acc) ys 
      | [] -> 
          let t' = Trie (None, []) in
          let t' = insert t' (String.sub w 1 (n - 1)) v in
          (h, t') :: List.rev acc
    in
    Trie (value, loop [] xs)
