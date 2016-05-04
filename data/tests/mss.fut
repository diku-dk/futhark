-- Parallel maximum segment sum
-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 }

fun int max(int x, int y) =
  if x > y then x else y

fun (int,int,int,int) redOp((int,int,int,int) x,
                            (int,int,int,int) y) =
  let (mssx, misx, mcsx, tsx) = x in
  let (mssy, misy, mcsy, tsy) = y in
  ( max(mssx, max(mssy, mcsx + misy))
  , max(misx, tsx+misy)
  , max(mcsy, mcsx+tsy)
  , tsx + tsy)

fun (int,int,int,int) mapOp (int x) =
  ( max(x,0)
  , max(x,0)
  , max(x,0)
  , x)

fun int main([int] xs) =
  let (x, _, _, _) =
    reduce(redOp, (0,0,0,0), map(mapOp, xs)) in
  x
