-- Positive test.  We can fulfill safety condition 2, since the memory for 'xs'
-- is allocated before 'ys' is created.
-- ==
-- input { 2
--         [[1, 1, 1],
--          [1, 1, 1],
--          [1, 1, 1]]
--         [5, 7, 9]
--       }
-- output { [[1, 1, 1],
--           [1, 1, 1],
--           [10, 14, 18]]
--        }
-- structure seq-mem { Alloc 0 }
-- structure gpu-mem { Alloc 0 }

def main [n] (i: i32) (xs: *[n][n]i32) (ys0: [n]i32) : [n][n]i32 =
  let ys = map (* 2) ys0
  -- Will use the memory of xs[i].
  let xs[i] = ys
  in xs
