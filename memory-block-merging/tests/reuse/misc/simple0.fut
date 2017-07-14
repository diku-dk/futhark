-- Very simple: Two independent arrays of the same size.
-- ==
-- input { [1, 3, 6]
--         1
--       }
-- output { [4, 4, 4]
--        }
-- structure cpu { Alloc 1 }

let main (xs0: [#n]i32, i: i32): [n]i32 =
  let xs = map (+ 1) xs0
  let k = xs[i]
  let ys = replicate n k -- Can be allocated into 'xs'.
  in ys
