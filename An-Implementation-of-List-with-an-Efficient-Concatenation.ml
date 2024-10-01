let rec to_list  = function
  | CSingle l -> [l]
  | CApp   (a, b) -> to_list a @ to_list b
  | CEmpty -> []
  
let rec of_list = function
  | [] -> CEmpty
  | [x] -> CSingle x
  | hd::tl -> CApp (CSingle hd, of_list tl)

let append l1 l2 = CApp (l1, l2)

let rec hd = function
  | CSingle x -> Some x
  | CApp (a, b) -> 
      let hda = hd a in
      if Option.is_some hda then hda
      else hd b
  | CEmpty -> None

let rec tl = function
  | CEmpty -> None
  | CSingle x -> Some CEmpty
  | CApp (CSingle _, b) -> Some b
  | CApp(CEmpty, b) -> tl b
  | CApp (CApp(x, y), b) -> 
      match tl x with
      | Some tlx -> Some (CApp(tlx, CApp(y, b)))
      | None -> match tl y with
        | Some tly -> Some (CApp(tly, b))
        | None -> match tl b with
          | Some tlb -> Some tlb
          | None -> None
