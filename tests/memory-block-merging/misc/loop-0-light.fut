-- Like loop-0.fut, but shorter and more explicit.  Register allocation
-- should not do anything to this.
-- ==
-- input { [[5, 6], [0, 0]]
--         [1, 2]
--       }
-- output { [[5, 9], [3, 4]]
--        }

import "/futlib/array"

-- Should result in 4 successful coalescing operations in the memory
-- block of 'y':
--
--   + y[1] = a2
--   + a2 = implicit copy a'
--   + a2 = loop memory of a
--   + a1 = copy a0

let main (y: *[#n][#m]i32, a0: [#m]i32): *[n][m]i32 =
  -- y: [[5, 6], [0, 0]]
  -- a0: [1, 2]
  let y[0, 1] = 9
  -- y: [[5, 9], [0, 0]]
  let a1 = copy a0
  -- a1: [1, 2]
  let a2 = loop a = a1 for _i < n do
    let a' = map (+ 1) a
    in a'
  -- a2: [3, 4]
  let y[1] = a2
  -- y: [[5, 9], [3, 4]]
  in y
