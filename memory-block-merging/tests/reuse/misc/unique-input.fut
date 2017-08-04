-- Reuse of an unique input parameter memory block.
-- ==
-- input { [1, 2, 3] }
-- output { [0, 2, 4] }
-- structure cpu { Alloc 1 }

let interfering_map (k: i32) (t: [#n]i32): [n]i32 =
  loop u = replicate n 0 for i < n - 1 do
    let u[i + 1] = t[i] + k
    in u

let main (x: *[#n]i32): [n]i32 =
  -- z interferes with x and will not reuse the memory of x.
  let z = interfering_map 1 x
  let k = z[1]
  -- v only interferes with z, not x, so it will reuse the memory of x.
  let v = interfering_map k z
  in v
