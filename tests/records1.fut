-- Tuples can be used like records.
-- ==
-- input { 2 } output { 3 1 }

let f(x: i32) = (x+1,x-1)

let main(x: i32) =
  let r = f x
  in (r.0, r.1)
