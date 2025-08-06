-- The compiler will try to coalesce y into x[0].  Since y is an if-expression,
-- the compiler will then try to coalesce y0 and b into y (the branch results).
-- At first this looks okay, since only one of them (b) is created outside the
-- if, so making y0 and b use the same memory should not conflict (like in
-- one-inside-one-outside.fut).  However, b is used *after* the creation of y0
-- (in b[0]), so if they are set to use the same memory block, the b[0]
-- expression will actually be y0[0], which is wrong.
--
-- This needs to fail at any coalescing.
-- ==
-- input { true
--         [[9, 9], [9, 9]]
--         [1, 4]
--       }
-- output { [[2, 5], [9, 9]]
--          2
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (cond: bool) (x: *[n][n]i32) (a: [n]i32) : (*[n][n]i32, i32) =
  let b = map (+ 1) a
  let (y, r) =
    if cond
    then let y0 = map (+ 1) a
         in (y0, b[0])
    else (b, 0)
  let x[0] = y
  in (x, r)
