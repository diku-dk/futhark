-- A simple program that requires a minimum of index analysis to observe that
-- ys_mem and xs_mem do *not* interfere, even though the ys expression reads
-- from xs.  The point is that the expression reads from and writes to the same
-- index.
-- ==
-- input { [5, 7]
--       }
-- output { [12, 14]
--        }
-- structure cpu { Alloc 1 }

let main (ns: [#n]i32): [n]i32 =
  let xs = map (+ 1) ns
  let k0 = xs[0]
  let ys = map (+ k0) xs
  in ys
