-- Concat-scatter fusion in a more complicated session.
-- ==
-- input { [0,5] }
-- output { [3, 5, 1, 1, 1, 3, 5, 1, 1, 1]
--          [4, 6, 2, 2, 2, 4, 6, 2, 2, 2] }
-- structure { Concat 0 Screma 1 }

def main [n] (xs: [n]i32) =
  let dest = replicate 10 (1, 2)
  let (is0, vs0, is1, vs1) =
    unzip4 (map (\x -> (i64.i32 x, (3, 4), i64.i32 x + 1, (5, 6))) xs)
  let m = n + n
  in unzip (scatter dest (concat is0 is1 :> [m]i64) (concat vs0 vs1 :> [m](i32, i32)))
