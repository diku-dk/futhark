-- Like plus.fut, but with three inputs, and with some index fiddlery (to try a
-- different approach).
-- ==
-- input { [3, 1] [2, 10] [5, 5] }
-- output { [10, 16] }
-- structure cpu { Alloc 0 }

let mapper (xs: []i32) (ys: []i32) (zs: []i32) (i: i32): i32 =
  unsafe (xs[i] + ys[i] + zs[i])

-- Input arrays of not necessarily the same size.
let main (xs: *[#n]i32, ys: []i32, zs: []i32): []i32 =
  -- The result of the map can be stored in the memory of 'xs' because of the
  -- straightforward index access patterns.
  map (mapper xs ys zs) (iota n)
