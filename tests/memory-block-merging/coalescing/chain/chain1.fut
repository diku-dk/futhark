-- Here is a chain of two in-place coalescings.  The compiler needs to keep
-- track of both slices to properly coalesce everything.
-- ==
-- input { [2, 4]
--         0
--         1
--       }
-- output { [[[2, 2],
--            [3, 5]],
--           [[1, 1],
--            [1, 1]]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n]
         (ns: [n]i32)
         (i: i32)
         (j: i32) : [n][n][n]i32 =
  -- Create arrays into which other arrays can be coalesced.  Two allocations,
  -- but both will use the same allocation after coalescing.
  let wsss = replicate n (replicate n (replicate n 1))
  let vss = replicate n (replicate n 2)
  -- Create the "base" array.
  let xs = map (+ 1) ns
  -- xs can be coalesced into vss[j].
  let vss[j] = xs
  -- vss (and thereby xs) can be coalesced into wsss[i].
  let wsss[i] = vss
  in wsss
