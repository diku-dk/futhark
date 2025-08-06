-- Simplification of concatenations of replicates of the same value,
-- interspersed with array literals.
-- ==
-- input { 2i64 3i64 }
-- output { [42i32, 42i32, 42i32, 42i32, 42i32, 1i32, 2i32, 3i32, 4i32, 5i32, 42i32, 42i32, 42i32] }

def main (n: i64) (m: i64) =
  replicate n 42 ++ replicate m 42 ++ [1, 2, 3] ++ [4, 5] ++ replicate n 42 ++ [42]
