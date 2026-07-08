-- Scan with array operator and not interchangeable.
-- ==
-- entry: scan_arr
-- random input { 100i64 100i64 [10000]i32 } auto output
-- random compiled input { 1000i64 1000i64 [1000000]i32 } auto output
-- random compiled input { 100000i64 10i64 [1000000]i32 } auto output
-- random compiled input { 1000000i64 100i64 [100000000]i32 } auto output
-- structure gpu { SegScan 1 }

def vecadd [n] (xs: [n]i32) (ys: [n]i32) : [n]i32 =
  loop res = #[scratch] replicate n 0
  for i < n do
    res with [i] = xs[i] + ys[i]

entry scan_arr (n: i64) (m: i64) (a: [n * m]i32) =
  scan vecadd (replicate m 0) (map (\x -> #[sequential] map (+ 3) x) (unflatten a))
