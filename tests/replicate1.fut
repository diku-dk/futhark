-- Simple test to see whether we can properly replicate arrays.  This
-- one sums the resulting array, to check very large ones.
-- ==
-- input { 20 } output { 3810 }
-- compiled no_python input { 2000 } output { -296967286i32 }
-- structure distributed { Replicate 1 }
let main(n: i32): i32 =
  let x  = iota n
  let y  = replicate n x
  -- Hack to force manifestation.
  let y[0,0] = 10
  in reduce (+) 0 (flatten y)
