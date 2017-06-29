-- A longer chain of non-overlapping memory blocks.
-- ==
-- input { [5, 7]
--         1
--       }
-- output { [15, 15]
--        }

-- structure cpu { Alloc 1 }

let main (ns: [#n]i32, i: i32): [n]i32 =
  let xs = map (+ 1) ns
  let k0 = xs[i]
  let ys = map (+ k0) ns -- Can end up in mem_xs.
  let k1 = ys[i]
  let zs = replicate n k1 -- Can end up in mem_ys, and by extension mem_xs.
  in zs
