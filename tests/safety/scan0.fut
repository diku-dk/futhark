-- ==
-- random input { [20000]f32 } error: Index \[-1\]

def main [n] (xs: [n]f32) =
  scan (+) 0 (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))

-- A failure in the map part of a scan, in a block other than the first. The
-- datasets are run in order by the same process, so the second one also checks
-- that the failure did not leave anything in a bad state.

-- ==
-- entry: fail_at
-- input { 30000i64 100000i64 } error: i != k
-- input { -1i64 100000i64 } output { 2500050000i64 9999900000i64 }

entry fail_at (k: i64) (n: i64) =
  let ys = scan (+) 0 (map (\i -> assert (i != k) (i * 2)) (iota n))
  in (ys[n / 2], ys[n - 1])

-- A failure after the scan, in its post-operation.

-- ==
-- entry: post_op
-- input { 100000i64 } error: y < 1000

entry post_op n = map (\y -> assert (y < 1000) (y * 2)) (scan (+) 0 (iota n))
