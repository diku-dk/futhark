-- ==
-- compiled random input { 10i64 [10][10][256]i64 } auto output
-- compiled random input { 10i64 [10][10][2048]i64 } auto output

def histogram k is =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) (map (const 1i32) is)

def main k = map (map (histogram k))
