-- Negative test.  'ys' aliases 'zs', so it already occupies another memory
-- block than "its own".
-- ==
-- input { [[2, 2],
--          [2, 2]]
--         [[3, 4],
--          [10, 20]]
--         0
--         0
--       }
-- output { [[9, 12],
--           [2, 2]]
--        }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (xs: *[n][n]i32) (zs0: [n][n]i32) (i: i32) (j: i32) : [n][n]i32 =
  let zs = map (\z -> map (* 3) z) zs0
  let ys = zs[i]
  let xs[j] = ys
  in xs
