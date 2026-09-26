-- Whole-array update of a multidimensional array with only a single
-- element must preserve the rank of the array.
-- ==
-- input { [[0i32]] 7i32 }
-- output { [[7i32]] }
-- structure { Update 0 }

entry main (x: *[1][1]i32) (v: i32) =
  #[unsafe] x with [0, 0] = v
