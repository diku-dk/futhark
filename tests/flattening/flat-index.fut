-- ==
-- input { [1i64, 2i64, 4i64]
--         [0i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64, 8i64, 9i64] }
-- auto output

entry main (ns: []i64) (xs: []i64) : []i64 =
  map (\n ->
         let ys = intrinsics.flat_index_2d xs 0 n 1 1 1
         in i64.sum (flatten ys))
      ns
