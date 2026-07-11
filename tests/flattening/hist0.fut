-- Essential structure: an outer `map` over a [n][m] array, and per row an
-- irregular (variable-length) array feeding a `hist`, then a `map` that indexes
-- that hist result, then a reduce.

def f [n] (keys0: [n]i64) : i64 =
  let xs = filter (\x -> x != 0) keys0
  let alloc_size = i64.max 4096 (length xs)
  let hashes = map (\k -> k %% alloc_size) xs
  let collision_idxs = hist i64.min i64.highest alloc_size hashes (indices hashes)
  let flags = map2 (\i h -> #[unsafe] collision_idxs[h] == i) (indices xs) hashes
  in i64.sum (map i64.bool flags)

entry main [n] [m] (arrs: [n][m]i64) : [n]i64 =
  map f arrs
