-- If both the indexes and values come from a concatenation of arrays
-- of the same size, that concatenation should be fused away.
-- ==
-- input { [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] [0, 5, 10] }
-- output { [1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 0] }
-- structure { Concat 0 Screma 1 }

def main [k] [n] (arr: *[k]i32) (xs: [n]i32) =
  let (a, b) =
    unzip (map (\x -> ((i64.i32 x, 1, i64.i32 x + 1, 2), (i64.i32 x + 2, i64.i32 x + 3, 3, 4))) xs)
  let m = n + n + n + n
  let ((is0, vs0, is1, vs1), (is2, is3, vs2, vs3)) = (unzip4 a, unzip4 b)
  in scatter arr (is0 ++ is1 ++ is2 ++ is3 :> [m]i64) (vs0 ++ vs1 ++ vs2 ++ vs3 :> [m]i32)
