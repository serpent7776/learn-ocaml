let exchange k =
  let a = k / 10 in
  let b = k mod 10 in
  b * 10 + a

let is_valid_answer (grand_father_age, grand_son_age) =
  let grand_father_exchanged_age = exchange grand_father_age in
  let grand_son_exchanged_age = exchange grand_son_age in
  grand_son_age * 4 = grand_father_age &&
  grand_father_exchanged_age * 3 = grand_son_exchanged_age

(*
let:
 f - father age
 s - son age
 f' - exchanged father age
 s' - exchanged son age
we have:
 4s = f and 3f'=s'
so:
 f | 4
 s' | 3 => s | 3 => f | 3 ?
 f | 4 and f | 4 => f | 12
 we can start at greatest f | 12 and go down to min
*)
let find (max_grand_father_age, min_grand_son_age) =
  let rec proc f min =
    if f < min then (-1, -1)
    else
      let s = f / 4 in
      let s' = exchange s in
      let f' = s' / 3 in
      let f'' = exchange f' in
      if f = f'' && s >= min then (f, s)
      else proc (f - 12) min
  in
  let max_divisible_by_12 = max_grand_father_age - max_grand_father_age mod 12
  in
  proc max_divisible_by_12 min_grand_son_age
