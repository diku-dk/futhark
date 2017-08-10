-- Memory block merging with a copy into a multidimensional array given as a
-- function parameter.
-- ==
-- input { [[0, 1, 2],
--          [0, 1, 2],
--          [0, 1, 2]]
--         1
--         [7, 0, 7]
--       }
-- output { [[0, 1, 2],
--           [8, 1, 8],
--           [0, 1, 2]]
--        }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

import "/futlib/array"

let main (t1: *[#n][#n]i32, i: i32, ns: [#n]i32): [n][n]i32 =
  let t0 = map (+ 1) ns -- Will use the memory of t1[i].

  -- This is the basis array in which everything will be put.
  let t1[i] = t0
  in t1
