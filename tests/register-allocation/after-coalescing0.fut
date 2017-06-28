-- Example of a program where running both memory block merging and register
-- allocation ends up reducing more allocations than running just one of them.
--
-- This should work no matter the order the passes are run in, since they touch
-- different parts of the program -- roughly speaking, the merging (coalescing)
-- occurs in the first half, and the register allocation-inspired merging occurs
-- in the latter half.
-- ==
-- input { [8, 9]
--         0
--       }
-- output { [[9, 10], [0, 0]]
--          [2, 2]
--        }

-- structure cpu { 1 }

import "/futlib/array"


let main (xs0: [#n]i32, i: i32): [n][n]i32 =
  -- This array (a single allocation) will end up containing all arrays of the
  -- function.
  let contents = replicate n (replicate n 0)

  -- 'xs' is coalesced into 'contents[i]'.
  let xs = map (+ 1) xs0
  let contents[i] = xs

  -- Last use of the 'contents' array.
  let contents_last_use = reduce (+) 0 (reshape (n * n) contents)

  -- Since this is after the last use of 'contents', it can be set to use its
  -- memory.
  let final = replicate n (replicate n contents_last_use)
  in final
