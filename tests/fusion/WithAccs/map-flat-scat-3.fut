-- Simple test for fusing scatter-flatten with the preceding
-- map nest that produces its indices and values
-- ==
-- tags { no_webgpu }
-- entry: main
-- input { 3i64 1i64 4i64 [[[12u32,11u32,10u32,9u32]], [[8u32,7u32,6u32,5u32]], [[4u32,3u32,2u32,1u32]]] }
-- output { [0u32, 1u32, 10u32, 45u32, 26u32, 62u32, 180u32, 81u32, 160u32, 405u32, 166u32, 302u32] }

def i64sqrt x = f64.i64 x |> f64.sqrt |> i64.f64

entry main (m: i64)
           (n: i64)
           (b: i64)
           (xss: *[m][n][b]u32) : *[m * n * b]u32 =
  let inds = map (map (map2 (\i x -> i64.u32 x * i64.u32 x + i) (iota b))) xss
  let vals = map (map (map (\x -> 5 * x * x))) xss
  let inds' = flatten inds |> flatten
  let vals' = flatten vals |> flatten
  let inds'' = map2 (\i x -> i64sqrt (x - (i % b))) (iota (m * n * b)) inds'
  let vals'' = map2 (\x i -> x / u32.i64 (i % 3 + 1)) vals' (iota (m * n * b))
  in scatter (replicate (m * n * b) 0u32) inds'' vals''
