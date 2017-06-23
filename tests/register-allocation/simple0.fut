-- Very simple: Two independent arrays of the same size.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 1 }

let main (n: i32, i: i32): [n]i32 =
  let xs = replicate n 1
  let k = xs[i]
  let ys = replicate n k
  in ys
