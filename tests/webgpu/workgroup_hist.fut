-- Test workgroup histogram in WebGPU.
-- Uses intra-group reduce_by_index which requires workgroup shared memory.

-- ==
-- random input { 10i64 [1][256]i64 } auto output
-- random input { 10i64 [10][256]i64 } auto output
-- random input { 100i64 [10][256]i64 } auto output

def histogram k is =
  hist (+) 0i32 k (map (% k) is) (map (const 1i32) is)

def main k is =
  #[incremental_flattening(only_intra)]
  map (histogram k) is
