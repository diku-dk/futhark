-- The internaliser logic for flattening out multidimensional array
-- literals was not reconstructing the original dimensions properly.

let insert 't (x: t) (a: []t) (i: i32): []t =
  let (b,c) = split i a
  in b ++ [x] ++ c

let list_insertions 't (x: t) (a: []t): [][]t =
  map (insert x a) (0...(length a))

let main [n] (a: [n][3]u8): [][n][3]u8 =
  loop p = [[head a]] for i in (1...n-1)
  do flatten (unsafe map (list_insertions a[i]) p)
