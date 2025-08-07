-- Positive test.  'xs' is not used while 'ys' is live, so we can merge their
-- memory blocks.
-- ==
-- input { [[2, 2],
--          [2, 2]]
--         [3, 4]
--         1
--       }
-- output { [[2, 2],
--           [4, 5]]
--          6
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (xs: *[n][n]i32) (ys0: [n]i32) (i: i32) : ([n][n]i32, i32) =
  let ys = map (+ 1) ys0
  -- Will use the memory of xs[i].
  let zs = map (+ 1) ys
  let xs[i] = ys
  in (xs, zs[i])
