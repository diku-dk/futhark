-- Just a longer chain of non-overlapping memory blocks.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 1 }

let main (n: i32, i: i32): [n]i32 =
  let xs = replicate (n + 1) 1
  let k0 = xs[i]
  let ys = replicate n k0
  let k1 = ys[i]
  let zs = replicate n k1
  in zs
