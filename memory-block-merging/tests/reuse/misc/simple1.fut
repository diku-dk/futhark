-- Requires that 'n' and '(shape xs)[0]' are recognized as the same value.  This
-- should be non-problematic for the compiler unless there are some layers of
-- indirection not removed by a simplifier.
-- ==
-- input { [1, 3, 6]
--         1
--       }
-- output { [4, 4, 4]
--        }
-- structure cpu { Alloc 1 }

let main (xs0: [#n]i32, i: i32): [n]i32 =
  let xs = map (+ 1) xs0
  let xs_length = (shape xs)[0]

  let k = xs[i]
  let ys = replicate xs_length k -- Can be allocated into 'xs'.

  in ys
