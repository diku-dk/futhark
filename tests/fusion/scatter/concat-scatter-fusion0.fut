-- If both the indexes and values come from a concatenation of arrays
-- of the same size, that concatenation should be fused away.
-- ==
-- input { [0,0,0,0,0,0,0,0] [0, 2, 6] }
-- output { [1, 2, 1, 2, 0, 0, 1, 2] }
-- structure { Concat 0 Screma 1 }

def main [k] [n] (arr: *[k]i32) (xs: [n]i32) =
  let (is0, vs0, is1, vs1) = unzip4 (map (\x -> (i64.i32 x, 1, i64.i32 x + 1, 2)) xs)
  let m = n + n
  in scatter arr (concat is0 is1 :> [m]i64) (concat vs0 vs1 :> [m]i32)
