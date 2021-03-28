-- Doesn't fuse, because the accumulator management functions as a fusion hindrance.
-- ==
-- input { 4i64 5i64 2i64 }
-- output {
-- [[0i64, 1i64], [1i64, 2i64], [2i64, 3i64], [3i64, 4i64]]
-- [[0, 0, 1, 1, 0], [0, 0, 1, 1, 1], [0, 0, 1, 1, 2], [0, 0, 1, 1, 3]]
-- }

import "intrinsics"

let f (acc: *acc ([]i32)) i =
  let acc = write acc (i*2) (i32.i64 i)
  let acc = write acc (i*2+1) (i32.i64 i)
  in acc

let main n m k =
  tabulate n (\i ->
                let xs = replicate m (i32.i64 i)
                in (map (+i) (iota k), scatter_stream xs f (iota k)))
  |> unzip
