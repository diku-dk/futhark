-- A replicate in a loop, with a subsequent loop after the replicate.  The
-- coalescing transformation must make sure *not* to coalesce the loop into the
-- return value.
--
-- This problem originated from the decision to allow allocations inside loops.
-- ==
-- input { [3, 6]
--         2i64
--       }
-- output { [5, 8]
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (a0: [n]i32) (n_iter: i64) : []i32 =
  let a2 =
    loop a = a0
    for _i < n_iter do
      -- If we coalesce a2 into a3, we end up coalescing the actual memory that
      -- the existential memory of a2 points to: the memory of b0.  But that
      -- memory is allocated inside the loop, and it needs to stay that way to
      -- ensure that a loop iteration can read from the memory of the previous
      -- iteration and write to the memory of the current iteration.  If a
      -- coalescing occurs, both iterations will use the same globally-created
      -- memory, and the replicate will write over everything written by the
      -- previous iteration.
      let b0 = replicate n 0i32
      let a' =
        loop b = b0
        for j < n do
          let b[j] = a[j] + 1
          in b
      in a'
  let a3 = copy a2
  in a3
