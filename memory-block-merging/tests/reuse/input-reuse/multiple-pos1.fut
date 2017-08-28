-- Similar to multiple-pos0.fut, just with an extra non-interfering array which
-- can also reuse the first memory block.
-- ==
-- input { [5, 7]
--       }
-- output { [24, 26]
--        }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main (ns: [#n]i32): [n]i32 =
  let xs = map (+ 1) ns
  let k0 = xs[0]
  let ys = map (+ k0) xs
  let k1 = ys[0]
  let zs = map (+ k1) ys
  in zs
