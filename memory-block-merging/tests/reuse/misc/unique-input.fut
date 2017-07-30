-- Reuse of an unique input parameter memory block.
-- ==
-- input { [1, 2] }
-- output { [4, 5] }
-- structure cpu { Alloc 1 }

let main (x: *[#n]i32): [n]i32 =
  -- z interferes with x and will not reuse the memory of x.
  let z = map (+ 1) x
  let k = z[0]
  -- v only interferes with z, not x, so it will reuse the memory of x.
  let v = map (+ k) z
  in v
