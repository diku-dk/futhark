-- http://rosettacode.org/wiki/100_doors
--
-- This is the "unoptimised" version, because the optimised one is too
-- boring.  Parametrised on number of doors.  One optimisation done is
-- to use write instead of a naive map.  This allows us to only touch
-- the doors we care about, while still remaining parallel.  0-indexes the doors.
--
-- ==
-- input { 10i64 }
-- output { [false, true, false, false, true, false, false, false, false, true] }
def main (n: i64): [n]bool =
  loop is_open = replicate n false for i < n do
    let js = map (* i + 1) (iota n)
    let flips =
      map
        (
          \j ->
            if j < n then
            notis_open[j]
            else
            true
        ) -- Doesn't matter.
        js
    in scatter is_open js flips