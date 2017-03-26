-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer type ascription.
--
-- ==
-- input { 2 [1,2,3] }
-- output { 4 }

let f ((_, elems: [n]i32): (i32,[]i32)): i32 =
  n + elems[0]

let main (x: i32) (y: []i32): i32 =
  f (x,y)
