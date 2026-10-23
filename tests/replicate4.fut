-- Strided slice of a replicate.
-- ==
-- input { 10i64 [1,2,3] }
-- output { [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]] }

def main (n: i64) (xs: [3]i32) =
  let r = replicate n xs
  in r[1:n:2]
