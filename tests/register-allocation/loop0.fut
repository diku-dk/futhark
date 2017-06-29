-- Pretty similar to simple0.fut, except in sequential code the final array map
-- expression is first-order-transformed into a loop, so in this case the
-- compiler pass also needs to handle loops.
-- ==
-- input { [1, 3, 6]
--         1
--       }
-- output { [4, 12, 24]
--        }

-- structure cpu { Alloc 1 }

let main (xs0: [#n]i32, i: i32): [n]i32 =
  let xs = map (+ 1) xs0
  let k = xs[i]
  let ys = map (* k) xs0 -- Can be allocated into 'xs'.
  in ys
