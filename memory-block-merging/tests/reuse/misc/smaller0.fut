-- The memory of 'xs' is larger than the memory of 'ys', so register allocation
-- should be possible.  Either the compiler should analyse the size
-- relationship, or it can just set the size of the first memory block to be the
-- maximum size of both memory blocks and be done with it -- this is possible
-- since they are in the same scope after allocation size hoisting has been
-- performed.
-- ==
-- input { [1, 3, 6]
--         1
--       }
-- output { [4, 4]
--        }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main [n] (xs0: [n]i32, i: i32): []i32 =
  let xs = map (+ 1) xs0
  let k = xs[i]
  let ys = replicate (n - 1) k
  in ys
