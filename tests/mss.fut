-- Parallel maximum segment sum
-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 }

def main (xs: []i32) : i32 =
  let max = i32.max
  let redOp (mssx, misx, mcsx, tsx) (mssy, misy, mcsy, tsy) =
    ( max mssx (max mssy (mcsx + misy))
    , max misx (tsx + misy)
    , max mcsy (mcsx + tsy)
    , tsx + tsy
    )
  let mapOp x =
    ( max x 0
    , max x 0
    , max x 0
    , x
    )
  in (reduce redOp (0, 0, 0, 0) (map mapOp xs)).0
