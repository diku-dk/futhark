-- The output memory of the inner body can reuse the input memory, but only if
-- the analysis is fairly full-fledged.
-- ==
-- input { [2, 5, 9] }
-- output { [5, 35, 117] }
-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let difficult (x: i32): i32 =
  -- The loop has the last use of x.
  let y = loop y = 0 for i < x do
    y + x + i

  -- y is the return value and can reuse the memory associated with x.
  in y

let main (xs: *[]i32): []i32 =
  map difficult xs
