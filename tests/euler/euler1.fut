-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-- ==
-- input { 1000i64 }
-- output { 233168i64 }

-- Approach: filter to get the numbers we are interested in, then sum
-- them.  Ideally this will be fused into a single loop.
def main (bound: i64) : i64 =
  reduce (+) 0 (filter (\(x: i64) : bool ->
                          x % 3 == 0 || x % 5 == 0)
                       (iota (bound)))
