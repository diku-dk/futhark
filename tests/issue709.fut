-- ==
-- input { 0 } output { [[[0]]] }

let insert 't (x: t) (a: []t) (i: i32): []t =
  let (b,c) = split i a   in b ++ [x] ++ c

let list_insertions 't (x: t) (a: []t): [][]t =
  map (insert x a) (0...(length a))

let main (x: i32) = map (list_insertions x) [[]]
