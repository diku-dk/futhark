-- Reshape and transpose inside a map. The reshape/rearrange interchange
-- must not confuse the outer map dimension with the reshaped dimensions.
-- ==
-- input { [[0i64, 1i64, 2i64, 3i64], [4i64, 5i64, 6i64, 7i64]] }
-- output { [[0i64, 2i64, 1i64, 3i64], [4i64, 6i64, 5i64, 7i64]] }
-- structure { Replicate 1 Reshape 2 Rearrange 1 }

entry main (x: [2][4]i64) =
  let y = map (\row -> unflatten (row :> [2 * 2]i64) :> [2][2]i64) x
  in map flatten (map transpose y)
