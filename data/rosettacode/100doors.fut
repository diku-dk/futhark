-- http://rosettacode.org/wiki/100_doors
--
-- This is the "unoptimised" version, because the optimised one is too
-- boring.  Parametrised on number of doors.  One optimisation done is
-- to use write instead of a naive map.  This allows us to only touch
-- the doors we care about, while still remaining parallel.  0-indexes the doors.
--
-- ==
-- input { 10 }
-- output { [False, True, False, False, True, False, False, False, False, True] }

fun main(n: int): [n]bool =
  let is_open = replicate n False
  loop (is_open) = for i < n do
    let js = map (*i+1) (iota n)
    let flips = map (fn j =>
                       if j < n
                       then unsafe !is_open[j]
                       else True -- Doesn't matter.
                    ) js
    in write js flips is_open
  in is_open
