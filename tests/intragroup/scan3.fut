-- Intra-group scan horizontally fused with map.
-- ==
-- random input { [256][256]f32 } auto output

let main xss =
  #[incremental_flattening(only_intra)]
  map (\xs -> (scan (+) 0f32 xs, map (+2) xs)) xss
  |> unzip
