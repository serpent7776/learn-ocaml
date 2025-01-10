let height n =
  let c = ref 0 in
  let rec f = function
    | Empty -> incr c; 0
    | Node (t, _, t') -> 1 + (max (f t) (f t'))
  in
  let r = f n in (r,!c)

let balanced n =
  let c = ref 0 in
  let rec f = function
    | Empty -> incr c; true
    | Node (t, _, t') ->
        (f t) && (f t') && (
          let (h, k) = height t in
          let (h', k') = height t' in
          c := !c+ k + k';
          h = h'
        )
  in
  let r = f n in (r, !c)

let bal_height bst =
  let c = ref 0 in
  let rec f = function
    | Empty -> incr c; 0
    | Node (t, _, t') ->
        let h = f t in
        let h' = f t' in
        if h = h' then 1 + h
        else raise (Unbalanced !c)
  in
  let r = f bst in (r, !c)

let balanced_fast bst =
  match bal_height bst with
  | (_, c) -> true, c
  | exception (Unbalanced c) -> false, c
