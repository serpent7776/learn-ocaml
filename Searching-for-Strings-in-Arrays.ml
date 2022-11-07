(* return true if a < b, false otherwise *)
let lesser a b =
  String.compare a b < 0
  
let find_pair f arr =
  let rec proc n =
    if n >= Array.length arr then true
    else f arr.(n - 1) arr.(n) && proc (n + 1) 
  in 
  if Array.length arr < 2 then true
  else proc 1

let is_sorted a =
  find_pair lesser a

let find dict word =
  let len = Array.length dict in
  let rec proc first last =
    if last - first = 1 then
      if String.compare dict.(first) word = 0 then first
      else -1
    else
      let i = (first + last) / 2 in
      let carry = (first + last) mod 2 in
      let e = dict.(i) in
      match String.compare e word with
      | 0 -> i
      | -1 -> proc i last
      | 1 -> proc first (i + carry)
      | _ -> failwith "invalid result code"
  in
  if len = 0 then -1
  else proc 0 len
