-- Should not short circuit because t3 is created after the others
-- ==
-- input { [5, 15]
--         0
--       }
-- output { [[6, 16, 10, 30, 1, 5],
--           [0,  0,  0,  0, 0, 0]]
--        }
-- structure seq-mem { Alloc 4 }
-- structure gpu-mem { Alloc 4 }

let main [n] (ns: [n]i32) (i: i32): [][]i32 =
  let t0 = map (+ 1) ns -- Will use the memory of t3.
  let t1 = map (* 2) ns -- Will use the memory of t3.
  let t2 = map (/ 3) ns -- Will use the memory of t3.
  let t3 = unflatten (replicate (n * (n+n+n)) 0)
  let t3[i] = concat (concat t0 t1) t2
  in t3
