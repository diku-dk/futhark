-- ==
-- random input { 10i64 [1][256]i64 } auto output
-- compiled random input { 10i64 [100][256]i64 } auto output

def histogram k is =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) (map (const 1i32) is)

def main k is = #[incremental_flattening(only_intra)] map (histogram k) is
