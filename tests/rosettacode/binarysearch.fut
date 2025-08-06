-- https://rosettacode.org/wiki/Binary_search
--
-- This is a straightforward translation of the imperative iterative solution.
--
-- ==
-- input { [1,2,3,4,5,6,8,9] 2 }
-- output { 1i64 }

def main [n] (as: [n]i32) (value: i32) : i64 =
  let low = 0
  let high = n - 1
  let (low, _) =
    loop ((low, high)) while low <= high do
      -- invariants: value > as[i] for all i < low
      --             value < as[i] for all i > high
      let mid = (low + high) / 2
      in if as[mid] > value
         then (low, mid - 1)
         else if as[mid] < value
         then (mid + 1, high)
         else (mid, mid - 1)
  -- Force termination.
  in low
