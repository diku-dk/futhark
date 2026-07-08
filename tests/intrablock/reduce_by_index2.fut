-- Nastier operator that requires locking.  (If we ever get 64-bit
-- float atomics, then maybe add another test.)
-- ==
-- random input { 10i64 [1][256]i64 } auto output
-- compiled random input { 10i64 [100][256]i64 } auto output

def histogram k is =
  hist (+) 0 k (map (% k) is) (map (const 1f64) is)

def main k is = #[incremental_flattening(only_intra)] map (histogram k) is
