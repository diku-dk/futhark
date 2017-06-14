-- Requires that 'n' and '(shape xs)[0]' are recognized as the same value.
-- ==

-- input {
--       }
-- output {
--        }
-- structure cpu { Alloc 1 }

let main (n: i32, i: i32): [n]i32 =
  let xs = iota n
  let xs_length = (shape xs)[0]

  let ys = replicate xs_length xs[0]

  in ys
