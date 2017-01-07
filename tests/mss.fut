-- Parallel maximum segment sum
-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 }

fun max(x: int) (y: int): int =
  if x > y then x else y

fun redOp((mssx, misx, mcsx, tsx): (int,int,int,int))
         ((mssy, misy, mcsy, tsy): (int,int,int,int)): (int,int,int,int) =
  ( max mssx (max mssy (mcsx + misy))
  , max misx (tsx+misy)
  , max mcsy (mcsx+tsy)
  , tsx + tsy)

fun mapOp (x: int): (int,int,int,int) =
  ( max x 0
  , max x 0
  , max x 0
  , x)

fun main(xs: []int): int =
  let (x, _, _, _) =
    reduce redOp (0,0,0,0) (map mapOp xs) in
  x
