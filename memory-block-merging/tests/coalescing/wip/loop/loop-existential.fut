-- A loop with an existential memory block.  The compiler should not coalesce
-- the existential memory block, but instead coalesce the aliased body result
-- memory (which is not existential).
-- ==
-- input { [3, 6]
--         2
--       }
-- output { [5, 8]
--        }

-- structure { Alloc 1 }

import "/futlib/array"

let main (a0: []i32, n_iter: i32): []i32 =
  let a2 = loop a = a0 for _i < n_iter do
    -- This map reads from the memory of 'a' and writes to the loop result
    -- memory.  mem_a aliases mem_result, which means that the compiler cannot
    -- naively coalesce mem_a' into mem_result, since then it would read and
    -- write to the same memory.  However, in this case it is okay, since a map
    -- can be in-place.  See loop-existential-loop.fut and
    -- loop-existential-loop-error for more explicit examples of this problem.
    let a' = map (+ 1) a
    in a'
  let a3 = copy a2
  in a3
