-- Simple histogram with u32.max operator
-- ==
-- compiled input {
--     [0i64,-1i64, 2i64, 1i64, 2i64, 2i64, 1i64,-1i64, 0i64] 
--     [1u32, 9u32, 5u32, 4u32, 7u32, 6u32, 5u32, 8u32, 3u32]
--     [4u32, 0u32, 0u32]
--     [2u32, 3u32, 4u32]
--   }
-- output { 
--     [32u32,15u32,28u32, 0u32,20u32,0u32,27u32, 0u32, 0u32]
--     [16u32,0u32, 0u32]
--   }
--

let histo_max [w][n] (is: [n]i64) (vs: [n]u32, hist0: [w]u32) : *[w]u32 =
  let hist0' = map2 (*) hist0 hist0
  let hist   = reduce_by_index hist0' (u32.max) (0u32) is vs
  in  map2 (*) hist (vs[0:w])

entry main [n][w] (is: [n]i64) (vs: [n]u32) (hist: [w]u32) (hist_bar: [w]u32) =
  vjp (histo_max is) (vs, hist) hist_bar