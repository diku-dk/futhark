-- Intra-group scan horizontally fused with map.
-- ==
-- random input { [256][256]f32 } auto output

def main xss =
  #[incremental_flattening(only_intra)]
  map (\xs -> (scan (+) 0f32 xs, map (+ 2) xs)) xss
  |> unzip
