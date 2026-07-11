-- Needs the loop to reproduce.

def f [n] (keys0: [n]i64) : i64 =
  let (acc, _) =
    loop (acc, xs: *[]i64) = (0, copy keys0)
    while length xs != 0 do
      let alloc_size = i64.max 4096 (length xs)
      let hashes = map (\k -> k %% alloc_size) xs
      let collision_idxs = hist i64.min i64.highest alloc_size hashes (indices hashes)
      let flags = map2 (\i h -> #[unsafe] collision_idxs[h] == i) (indices xs) hashes
      let scan_res = flags |> map i64.bool |> scan (+) 0
      let count =
        scatter (replicate 1 0)
                (map (\i -> if i == length scan_res - 1 then 0 else -1) (indices scan_res))
                scan_res
      let continue_size = count[0]
      in (acc + continue_size, xs[:continue_size])
  in acc

entry main [n] [m] (arrs: [n][m]i64) : [n]i64 =
  map f arrs
