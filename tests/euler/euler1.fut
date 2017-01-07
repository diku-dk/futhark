-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- ==
-- input { 1000 }
-- output { 233168 }

-- Approach: filter to get the numbers we are interested in, then sum
-- them.  Ideally this will be fused into a single loop.
fun main(bound: int): int =
  reduce (+) 0 (
         filter (fn (x: int): bool  =>
                  x % 3 == 0 || x % 5 == 0) (
                iota(bound)))
