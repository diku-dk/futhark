-- Simplification of concatenations of replicates of the same value,
-- interspersed with array literals.
-- ==
-- input { 2 3 }
-- output { [42i32, 42i32, 42i32, 42i32, 42i32, 1i32, 2i32, 3i32, 4i32, 5i32, 42i32, 42i32, 42i32] }

let main (n: i32) (m: i32) =
  replicate n 42 ++ replicate m 42 ++ [1,2,3] ++ [4,5] ++ replicate n 42 ++ [42]
