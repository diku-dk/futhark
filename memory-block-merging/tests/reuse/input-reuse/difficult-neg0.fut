-- Not all allocations can be removed in this program.
-- ==
-- input { [2, 5, 9] }
-- output { [8, 14, 22] }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main [n] (xs: *[n]i32): [n]i32 =
  -- xs and ys do not overlap "within" the map, but since xs is used again in a
  -- later statement, they do actually interfere.
  let ys = map (+ 1) xs
  let k = ys[0]
  let zs = map (\x y -> x + y + k) xs ys
  in zs
