-- The memory of 'xs' is larger than the memory of 'ys', so register allocation
-- should be possible.  The compiler must first analyse the size relationship.
-- ==
-- input { [1, 3, 6]
--         1
--       }
-- output { [4, 4]
--        }

-- structure cpu { Alloc 1 }


let main (xs0: [#n]i32, i: i32): []i32 =
  let xs = map (+ 1) xs0
  let k = xs[i]
  let ys = replicate (n - 1) k
  in ys
