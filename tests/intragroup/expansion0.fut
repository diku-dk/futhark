-- This uses way too much memory if memory expansion is done by the
-- number of threads, not the number of groups.
-- ==
-- random input { [30]bool [30][2]f32 }
-- compiled random input { [30000]bool [30000][256]f32 }

def main bs xss =
  #[incremental_flattening(only_intra)]
  map2 (\b xs -> if b then xs else iterate 10 (scan (+) 0f32) xs) bs xss
