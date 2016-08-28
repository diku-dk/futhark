-- Simple test to see whether we can properly replicate arrays.  This
-- one sums the resulting array, to check very large ones.
-- ==
-- input { 20 } output { 3810 }
-- compiled input { 2000 } output { -296967286i32 }
-- structure distributed { Replicate 1 }
fun main(n: int): int =
  let x  = iota n
  let y  = replicate n x
  -- Hack to force manifestation.
  let y[0,0] = 10
  in reduce((+), 0, reshape (n*n) y)
