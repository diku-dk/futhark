-- ==
-- input { [0i64, 0i64, 0i64] [34i64,5i64, 9i64] }
-- auto output

entry main (xs : []i64) (ys : []i64) =
  map
    (\x ->
      let d = reduce (+) x ys
      in d + x
    )
    xs
