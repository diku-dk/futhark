-- Memory block merging with a copy into a multidimensional array.  Requires
-- allocation hoisting of the memory block for 't1'.
-- ==
-- input { 1i64
--         [7i64, 0i64, 7i64]
--       }
-- output { [[0i64, 1i64, 2i64],
--           [8i64, 1i64, 8i64],
--           [0i64, 1i64, 2i64]]
--        }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 2 }

def main [n] (i: i64) (ns: [n]i64) : [n][n]i64 =
  let t1 = replicate n (iota n)
  let t0 = map (+ 1) ns
  -- Will use the memory of t1[i].

  -- This is the basis array in which everything will be put.  Its creation uses
  -- two allocations.
  let t1[i] = t0
  in t1
