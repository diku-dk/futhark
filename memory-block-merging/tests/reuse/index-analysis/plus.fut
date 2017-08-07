-- We can get away with having zero allocations in this case because of the
-- straightforward access pattern in a '(+)'.
-- ==
-- input { [3, 1] [2, 10] }
-- output { [5, 11] }
-- structure cpu { Alloc 0 }

let main (xs: *[#n]i32, ys: [#n]i32): *[n]i32 =
  map (+) xs ys
