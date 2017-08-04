-- Requires performing index analysis on perfectly nested maps.
-- ==
-- input { [[1], [2]]
--       }
-- output { [[4], [5]]
--        }

-- structure cpu { Alloc 1 }

let main (ns: [][]i32): [][]i32 =
  let xs = map (\n -> map (+ 1) n) ns
  let k0 = xs[0, 0]
  let ys = map (\x -> map (+ k0) x) xs
  in ys
