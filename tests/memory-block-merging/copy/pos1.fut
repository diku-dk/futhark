-- Memory block merging with a copy into a multidimensional array.  Requires
-- allocation hoisting of the memory block for 't1'.
-- ==
-- input { 1
--         [7, 0, 7]
--       }
-- output { [[0, 1, 2],
--           [8, 1, 8],
--           [0, 1, 2]]
--        }

import "/futlib/array"

let main (i: i32, ns: [#n]i32): [n][n]i32 =
  let t0 = map (+ 1) ns
  let t1 = replicate n (iota n)
  let t1[i] = t0
  in t1
