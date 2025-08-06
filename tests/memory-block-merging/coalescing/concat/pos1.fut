-- Memory block merging with a concat of multiple arrays into a multidimensional
-- array.  Requires allocation hoisting of the memory block for 't3'.
-- ==
-- input { [5, 15]
--         0i64
--       }
-- output { [[6, 16, 10, 30, 1, 5],
--           [0,  0,  0,  0, 0, 0]]
--        }
-- structure seq-mem { Alloc 3 }
-- structure gpu-mem { Alloc 1 }

def main [n] (ns: [n]i32) (i: i64) : [][]i32 =
  let t3 = unflatten (replicate (n * (n + n + n)) 0)
  let t0 = map (+ 1) ns
  -- Will use the memory of t3.
  let t1 = map (* 2) ns
  -- Will use the memory of t3.
  let t2 = map (/ 3) ns
  -- Will use the memory of t3.
  let t3[i] = concat (concat t0 t1) t2
  in t3
