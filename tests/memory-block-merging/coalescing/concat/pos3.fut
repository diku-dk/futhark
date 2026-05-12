-- Memory block merging with a concat of two arrays, but in the different order
-- than they were created.
-- ==
-- input { [5, 15] }
-- output { [10, 30, 6, 16] }
-- structure seq-mem { Alloc 2 }
-- structure gpu-mem { Alloc 1 }

def main (ns: []i32) : []i32 =
  let t0 = map (+ 1) ns
  let t1 = map (* 2) ns
  let t2 = concat t1 t0
  in t2
