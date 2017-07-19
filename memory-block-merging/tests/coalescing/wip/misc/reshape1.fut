-- Memory block merging with a copy and a reshape and in-place updating.
--
-- FIXME: There is some index function mess that needs fixing in the compiler
-- for this to work.
-- ==
-- input { [ [[-1, -2, -3, -4]]
--         , [[-5, -6, -7, -8]]
--         ]
--         [1, 2, 3, 4]
--       }
-- output { [ [[-1, -2, -3, -4]]
--          , [[2, 3, 4, 5]]
--          ]
--        }

-- structure cpu { Alloc 0 }

import "/futlib/array"

let main (t1: *[2][1][#n]i32, ns: [#n]i32): [2][1][n]i32 =
  -- Will both use the memory of t1.
  let t0 = map (+ 1) ns
  let t0a = reshape (1, n) t0

  let t1[1] = t0a
  in t1
