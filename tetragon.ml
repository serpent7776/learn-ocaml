let rec distinct = function
  | [] -> true
  | hd :: tl ->
      not @@ List.mem hd tl && distinct tl

let pairwise_distinct (lup, rup, llp, rlp) =
  distinct [lup; rup; llp; rlp]

let wellformed ((lux, luy), (rux, ruy), (llx, lly), (rlx, rly)) =
  lux < rux && lux < rlx &&
  llx < rux && llx < rlx &&
  luy > lly &&
  ruy > rly

let rotate_point (x, y) =
  (y, -x)
  
let sort_x (p, q) =
  let (px, _) = p in
  let (qx, _) = q in
  if px < qx then (p, q)
  else if qx < px then (q, p)
  else (p, q)

let by_y (_, py) (_, qy) = qy - py
    
let sort_y ps = List.sort by_y ps
      
let reorder (p1, p2, p3, p4) =
  let [q1; q2; q3; q4] = sort_y [p1; p2; p3; p4] in
  let r1, r2 = sort_x (q1, q2) in
  let r3, r4 = sort_x (q3, q4) in
  (r1, r2, r3, r4)

let rotate_tetragon (lup, rup, llp, rlp) =
  (
    rotate_point llp,
    rotate_point lup,
    rotate_point rlp,
    rotate_point rup
  )
