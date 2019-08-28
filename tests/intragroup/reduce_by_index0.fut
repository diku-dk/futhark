-- ==
-- compiled random input { 10 [100][256]i32 } auto output
-- compiled random input { 10 [100][2048]i32 } auto output

let histogram k is =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) (map (const 1i32) is)

let main k = map (histogram k)
