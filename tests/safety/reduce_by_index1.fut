-- ==
-- compiled random input { [20000]f32 } error: l != 1337

let main [n] (xs: *[n]f32) =
  let xs[1337] = f32.lowest
  let op i j =
    let l = if xs[i] < xs[j] then i else j
    in assert (l != 1337) l
  in reduce_by_index (replicate 3 0) op 0 (map (%3) (iota n)) (iota n)
