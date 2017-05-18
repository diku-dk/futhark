-- Memory block merging with a concat of multiple arrays.  Requires allocation
-- hoisting of the memory block for 't3'.
-- ==
-- input { [5, 15] }
-- output { [6, 16, 10, 30, 1, 5] }

-- structure cpu { Alloc 1 }

let main (ns: []i32): []i32 =
  let t0 = map (+ 1) ns -- Will use the memory of t3.
  let t1 = map (* 2) ns -- Will use the memory of t3.
  let t2 = map (/ 3) ns -- Will use the memory of t3.
  let t3 = concat t0 t1 t2
  in t3
