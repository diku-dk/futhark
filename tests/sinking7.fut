-- #2623
-- ==
-- input { [0i64, 3i64, 5i64] } output { [0i64, 8i64, 23i64] }
-- input { [-1i64] } error: r >= 0

entry main (rs: []i64) =
  map (\r ->
         let m = assert (r >= 0) r
         in reduce (+)
                   0
                   (map (\x ->
                           loop s = x
                           for k < m do
                             if k > s then s + m else s - k)
                        (iota m)))
      rs
