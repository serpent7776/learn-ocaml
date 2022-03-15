let move p dp = {
  x = p.x +. dp.dx;
  y = p.y +. dp.dy;
  z = p.z +. dp.dz};;

let next o =
  {o with position = move o.position o.velocity};;

let dist2 p q =
  let x = p.x -. q.x in
  let y = p.y -. q.y in
  let z = p.z -. q.z in
  x*.x +. y*.y +. z*.z
;;

let will_collide_soon p1 p2 =
  let q1 = next p1 in
  let q2 = next p2 in
  dist2 q1.position q2.position < 4.0
;;
