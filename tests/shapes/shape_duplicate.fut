-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer named shape.
--
-- ==
-- input { 2 [1,2,3] }
-- output { 7 }

let f ((_, elems: [n]i32): (i32,[m]i32)): i32 =
  n + m + elems[0]

let main (x: i32, y: []i32): i32 = f (x, y)
