-- Like 3op.fut, but with two outputs.
-- ==
-- input { [3, 1] [2, 10] [5, 5] }
-- output { [10, 16] [30, 50] }

-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

let mapper (xs: []i32) (ys: []i32) (zs: []i32) (i: i32): (i32, i32) =
  (xs[i] + ys[i] + zs[i], xs[i] * ys[i] * zs[i])

-- Input arrays of not necessarily the same size (for zs at least).
let main (xs: *[#n]i32, ys: *[#n]i32, zs: []i32): ([]i32, []i32) =
  -- The result of the map can be stored in the memory of 'xs' and 'ys' because
  -- of the straightforward index access patterns.
  unzip (map (mapper xs ys zs) (iota n))
