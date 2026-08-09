-- Basic flatmap, but multidimensional: expand each element into 'k' copies of
-- itself, and check that the data and all the metadata arrays are correct.  The
-- metadata is in units of the (non-scalar) element type, not scalars.
-- ==
-- input { [0i64, 2i64, 3i64, 1i64] [[5i32, 6i32], [10i32, 11i32], [20i32, 21i32], [30i32, 31i32]] }
-- output { [0i64, 2i64, 3i64, 1i64]
--          [true, false, true, false, false, true]
--          [0i64, 0i64, 2i64, 5i64]
--          [[10i32, 11i32], [10i32, 11i32],
--           [20i32, 21i32], [20i32, 21i32], [20i32, 21i32],
--           [30i32, 31i32]] }

def main (ks: []i64) (xs: [][]i32) =
  flatmap (\(k, x) -> replicate k x) (zip ks xs)
