-- ==
-- input { 0 } output { [[[0]]] }

let insert [n] 't (np1: i32) (x: t) (a: [n]t) (i: i32): [np1]t =
  let (b,c) = split i a
  in b ++ [x] ++ c :> [np1]t

let list_insertions [n] 't (np1: i32) (x: t) (a: [n]t): [np1][np1]t =
  map (insert np1 x a) (0...(length a)) :> [np1][np1]t

let main (x: i32) = map (list_insertions 1 x) [[]]
