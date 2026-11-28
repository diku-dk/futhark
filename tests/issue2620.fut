-- ==
-- entry: top_scan
-- input { true [1i64, 2i64] } output { [1i64, 3i64] }
-- input { false [1i64, 2i64] } error: precondition_ok

entry top_scan (precondition_ok: bool) (xs: []i64) : []i64 =
  let xs = assert precondition_ok xs
  in scan (+) 0i64 xs

-- ==
-- entry: top_reduce
-- input { true [1i64, 2i64] } output { 3i64 }
-- input { false [1i64, 2i64] } error: precondition_ok

entry top_reduce (precondition_ok: bool) (xs: []i64) : i64 =
  let xs = assert precondition_ok xs
  in reduce (+) 0i64 xs

-- ==
-- entry: nested_scan
-- input { [true, true] [[1i64, 2i64], [3i64, 4i64]] }
-- output { [[1i64, 3i64], [3i64, 7i64]] }
-- input { [true, false] [[1i64, 2i64], [3i64, 4i64]] }
-- error: precondition_ok

entry nested_scan (oks: []bool) (xss: [][]i64) : [][]i64 =
  map2 (\precondition_ok xs -> let xs = assert precondition_ok xs in scan (+) 0i64 xs) oks xss

-- ==
-- entry: nested_replicate
-- input { [true, true] [1i64, 2i64] }
-- output { [[1i64, 1i64], [2i64, 2i64]] }
-- input { [true, false] [1i64, 2i64] }
-- error: precondition_ok

entry nested_replicate (oks: []bool) (xs: []i64) : [][2]i64 =
  map2 (\precondition_ok x -> let x = assert precondition_ok x in replicate 2i64 x) oks xs
