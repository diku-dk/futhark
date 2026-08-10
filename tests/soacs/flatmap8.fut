-- A flatmap whose lambda produces arrays of constant size. The size is still
-- existential as far as the flatmap is concerned, so internalisation must bind
-- the constant to a variable and coerce the result to it - otherwise the result
-- would be indistinguishable from the value result. Note that the lambda must be
-- written here, rather than passed to flatmap as a variable, as otherwise the
-- constant is hidden behind an application whose result size is already a
-- variable.
-- ==
-- input { [1i32, 2i32, 3i32] }
-- output { [3i64, 3i64, 3i64]
--          [true, false, false, true, false, false, true, false, false]
--          [0i64, 3i64, 6i64]
--          [1i32, 1i32, 1i32, 2i32, 2i32, 2i32, 3i32, 3i32, 3i32]
--          [2i32, 4i32, 6i32] }
-- input { empty([0]i32) }
-- output { empty([0]i64)
--          empty([0]bool)
--          empty([0]i64)
--          empty([0]i32)
--          empty([0]i32) }

def main (xs: []i32) =
  flatmap (\x -> (replicate 3 x, x * 2)) xs
