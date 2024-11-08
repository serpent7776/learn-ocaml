let ccr = fun a ->
  let ha = a /. 2.0 in
  let cosha = 8.0 *. cos ha in
  fun b ->
    let hb = b /. 2.0 in
    let coshb = cos hb in
    let cosab = cosha *. coshb in
    fun c -> 
      let hc = c /. 2.0 in
      let coshc = cos hc in
      let cosabc = cosab *. coshc in
      fun s ->
        s /. cosabc
