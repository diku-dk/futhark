-- The internaliser logic for flattening out multidimensional array
-- literals was not reconstructing the original dimensions properly.

let insert [n] 't (np1: i32) (x: t) (a: [n]t) (i: i32): [np1]t =
  let (b,c) = split i a
  in b ++ [x] ++ c :> [np1]t

let list_insertions [n] 't (np1: i32) (x: t) (a: [n]t): [n][np1]t =
  map (insert np1 x a) (iota n)

let main [n] (a: [n][3]u8): [][n][3]u8 =
  (loop p = [[head a]] for i in (1...n-1)
   do flatten (unsafe map (list_insertions (n+1) a[i]) p)) :> [][n][3]u8
