-- ==
-- compiled random input { 10i64 [100][256]i64 } auto output
-- compiled random input { 10i64 [100][2048]i64 } auto output

let histogram k is =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) (map (const 1i32) is)

let main k = map (histogram k)
