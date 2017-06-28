-- The memory of 'zs' can be allocated into either the memory of 'xs' or 'ys'.
-- The compiler just needs to make a choice.
-- ==
-- input { [2, 3]
--         [9, 9]
--         0
--       }
-- output { [31, 31]
--        }

-- structure cpu { Alloc 2 }

let main (xs0: [#n]i32, ys0: [#n]i32, i: i32): [n]i32 =
  let xs = map (* 2) xs0
  let ys = map (* 3) ys0
  let k = xs[i] + ys[i]
  let zs = replicate n k
  in zs
