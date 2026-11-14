-- ==
-- random input { [20000]f32 } error: Index \[-1\]

def main [n] (xs: [n]f32) =
  f32.sum (map (\i -> xs[if i == 1000 then -1 else i]) (iota n))

-- A failure in a block other than the first. The datasets are run in order by
-- the same process, so the second one checks that the failure did not leave
-- the counters used for the final reduction in a bad state.

-- ==
-- entry: fail_at
-- input { 30000i64 100000i64 } error: i != k
-- input { -1i64 100000i64 } output { 9999900000i64 }

entry fail_at (k: i64) (n: i64) =
  i64.sum (map (\i -> assert (i != k) (i * 2)) (iota n))
