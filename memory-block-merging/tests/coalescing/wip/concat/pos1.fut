-- Memory block merging with a concat of multiple arrays into a multidimensional
-- array.  Requires allocation hoisting of the memory block for 't3'.
--
-- FIXME: This depends on better reshape coalescing support.
-- ==
-- input { [5, 15]
--         0
--       }
-- output { [[6, 16, 10, 30, 1, 5],
--           [0,  0,  0,  0, 0, 0]]
--        }

-- structure cpu { Alloc 1 }

let main (ns: [#n]i32, i: i32): [][]i32 =
  let t0 = map (+ 1) ns -- Will use the memory of t3.
  let t1 = map (* 2) ns -- Will use the memory of t3.
  let t2 = map (/ 3) ns -- Will use the memory of t3.
  let t3 = reshape (n, n * 3) (replicate (n * n * 3) 0)
  let t3[i] = concat t0 t1 t2
  in t3
