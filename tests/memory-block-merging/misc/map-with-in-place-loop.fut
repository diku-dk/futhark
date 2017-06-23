-- A tricky case for array coalescing.
-- ==

-- structure cpu { Alloc 1 }

import "/futlib/array"

let mapper (_arg: i32): []i32 =
  let xs = replicate 1 0

  -- Will be merged.
  in loop (xs) for _i < 2 do
       let xs[0] = xs[0]
       in xs

let main (ys: []i32): [][]i32 =
  map mapper ys
