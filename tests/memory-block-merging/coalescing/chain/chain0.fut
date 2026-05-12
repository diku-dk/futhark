-- Memory block merging with a chain of two copies (the second copy is
-- technically a concat, but mostly acts as a copy).  Requires allocation
-- hoisting.
-- ==
-- input { [7, 0, 7] }
-- output { [7, 0, 7, 8, 1, 8] }
-- structure seq-mem { Alloc 1 }
-- structure gpu-mem { Alloc 1 }

def main [n] (ns: [n]i32) : []i32 =
  -- Will initially be set to use the memory of t1.  Will end up using the
  -- memory of t2 through t1.
  let t0 = map (+ 1) ns
  -- Will use the second part of the memory of t2.
  let t1 = copy t0
  -- Will be the only remaining memory block.
  let t2 = concat ns t1
  in t2
