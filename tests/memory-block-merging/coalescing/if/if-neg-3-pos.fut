-- Positive variant of if-neg-3.fut: b[0] is used in the then-branch, but before
-- y0 is created, so it is okay to coalesce both b and y0 into x[0] (through the
-- existential memory of y).
--
-- This also shows that it might make sense to do the memory block merging pass
-- in a different representation: This program calculates the exact same thing
-- as if-neg-3.fut, and yet this one is able to do two more coalescings just by
-- moving a statement.
--
-- However, existentials in ifs are not supported yet.
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
    then let k = b[0]
         let y0 = map (+ 1) a
         in (y0, k)
    else (b, 0)
  let x[0] = y
  in (x, r)
