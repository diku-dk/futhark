-- The memory of 'xs' is larger than the memory of 'ys', so register allocation
-- should be possible.  The compiler must first analyse the size relationship.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 1 }

let main (n: i32, i: i32): [n]i32 =
  let xs = replicate (n + 1) 1
  let k = xs[i]
  let ys = replicate n k
  in ys
