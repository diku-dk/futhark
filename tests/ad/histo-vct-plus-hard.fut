-- Simple histogram with vector addition
-- ==

let histo_vct_plus [n][d][w] (is: [n]i64) (xs: [n][d]f32, hist: [w][d]f32) : [w][d]f32 =
  let hist' = map2 (map2 (*)) hist hist
  let hist'' = reduce_by_index hist' (map2 (+)) (replicate d 0f32) is xs
  in  map2 (map2 (*)) hist'' hist''

entry main [n][d][w] (is: [n]i64) (xss: [n][d]f32) (hist: [w][d]f32) (hist_bar: [w][d]f32) =
  vjp (histo_vct_plus is) (xss, hist) hist_bar