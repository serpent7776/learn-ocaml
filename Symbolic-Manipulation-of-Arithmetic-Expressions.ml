let my_example =
  EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))

let rec eval = function
  | EInt x -> x
  | EAdd (x,y) -> eval x + eval y
  | EMul (x, y) -> eval x * eval y

let factorize = function
  | EAdd (EMul (a, b), EMul(c, d)) as e ->
      if a = c then EMul(a, EAdd(b, d))
      else e
  | e -> e

let expand = function
  | EMul (a, EAdd(b, c)) ->
      EAdd(EMul(a, b), EMul(a, c))
  | e -> e

let simplify = function
  | EMul (a, EInt 0) -> EInt 0
  | EMul (a, EInt 1) -> a
  | EAdd (a, EInt 0) -> a
  | EMul (EInt 0, a) -> EInt 0
  | EMul (EInt 1, a) -> a
  | EAdd (EInt 0, a) -> a
  | e -> e
