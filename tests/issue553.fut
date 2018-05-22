-- Can we handle really large tuples correctly?
-- ==
-- input { 1 2 3 4 5 6 7 8 9 10 }
-- output { 1 2 3 }

let main (x1: i32, x2: i32, x3: i32, x4: i32, x5: i32, x6: i32, x7: i32, x8: i32, x9: i32, x10: i32) =
  let t = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  in (t.1, t.2, t.3)
