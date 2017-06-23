-- The memory of 'zs' can be allocated into either the memory of 'xs' or 'ys'.
-- The compiler just needs to make a choice.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 2 }

let main (n: i32, i: i32): [n]i32 =
  let xs = replicate n 1
  let ys = replicate n 2
  let k = xs[i] + ys[i]
  let zs = replicate n k
  in zs
