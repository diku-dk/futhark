-- Memory block merging with a concat of multiple arrays.  Requires allocation
-- hoisting of the memory block for 't3'.
-- ==
-- input { [5, 15] }
-- output { [6, 16, 10, 30, 1, 5] }
-- structure seq-mem { Alloc 3 }
-- structure gpu-mem { Alloc 1 }

def main (ns: []i32) : []i32 =
  let t0 = map (+ 1) ns
  -- Will use the memory of t4.
  let t1 = map (* 2) ns
  -- Will use the memory of t4.
  let t2 = map (/ 3) ns
  -- Will use the memory of t4.
  let t3 = concat t0 t1
  -- Will use the memory of t4.
  let t4 = concat t3 t2
  in t4
