-- ==
-- entry: index_antidiag
-- input { [  0i64,  1i64,  2i64,  3i64,
--            4i64,  5i64,  6i64,  7i64,
--            8i64,  9i64, 10i64, 11i64,
--           12i64, 13i64, 14i64, 15i64 ] }
-- output { [[[2i64, 3i64], [6i64, 7i64]],
--           [[5i64, 6i64], [9i64, 10i64]],
--           [[8i64, 9i64], [12i64, 13i64]]] }
-- input { [ 0i64 ] }
-- error: out of bounds

-- ==
-- entry: update_antidiag
-- input { [  0i64,  1i64,  2i64,  3i64,
--            4i64,  5i64,  6i64,  7i64,
--            8i64,  9i64, 10i64, 11i64,
--           12i64, 13i64, 14i64, 15i64 ] }
-- output { [ 0i64,  0i64,  1i64,  3i64,
--            4i64,  2i64,  3i64,  7i64,
--            8i64,  9i64,  4i64,  5i64,
--           12i64, 13i64,  6i64,  7i64 ] }
-- input { [ 0i64 ] }
-- error: out of bounds

import "intrinsics"

entry index_antidiag [n] (xs: [n]i64) : [][][]i64 =
  flat_index_3d xs 2 3 3 2 4 2 1

entry update_antidiag [n] (xs: *[n]i64) : *[n]i64 =
  let vs = iota (2 * 2 * 2) |> unflatten |> unflatten
  in flat_update_3d xs 1 9 4 1 vs
