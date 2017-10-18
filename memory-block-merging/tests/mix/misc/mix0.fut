-- Example of a program where running both memory block merging and register
-- allocation ends up reducing more allocations than running just one of them.
--
-- This should work no matter the order the passes are run in, since they touch
-- different parts of the program -- roughly speaking, the coalescing merging
-- occurs in the first half, and the register allocation-inspired reuse merging
-- occurs in the latter half.
--
-- In the CPU pipeline, we should get a reduction from 3 allocations to 1
-- allocation.  In the GPU pipeline there are some auxiliary arrays, but we
-- should get the same number of reductions, in this case from 7 to 5 (you need
-- to check this manually).
-- ==
-- input { [8, 9]
--         0
--       }
-- output { [[19, 19], [19, 19]]
--        }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 5 }

import "/futlib/array"


let main [n] (xs0: [n]i32, i: i32): [n][n]i32 =
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
