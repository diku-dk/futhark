-- Simple histogram with multiplication
-- ==
--
-- compiled input { [1i64, 3i64, 2i64,-1i64, 2i64, 1i64, 1i64, 2i64, 3i64, 2i64,-1i64, 2i64, 2i64]
--				    [1f32, 3f32, 2f32, 1f32, 2f32, 1f32, 1f32, 2f32, 3f32, 2f32, 1f32, 2f32, 2f32]
--				    [4f32, 3f32, 2f32, 1f32]
--				    [9f32, 8f32, 7f32, 5f32]
--				  }
-- output {         [ 72f32, 15f32, 896f32, 0f32, 896f32, 72f32, 72f32, 896f32, 15f32, 896f32, 0f32, 896f32, 896f32 ]
--					[ 72f32, 48f32, 1792f32, 90f32 ]
--        }
--
-- compiled input { [1i64, 3i64, 2i64,-1i64, 2i64, 1i64, 1i64, 2i64, 3i64, 2i64,-1i64, 2i64, 2i64]
--				    [1f32, 3f32, 2f32, 1f32, 2f32, 1f32, 1f32, 2f32, 3f32, 2f32, 1f32, 0f32, 2f32]
--				    [4f32, 3f32, 2f32, 1f32]
--				    [9f32, 8f32, 7f32, 5f32]
--				  }
-- output {        [ 72f32, 15f32, 0f32, 0f32, 0f32, 72f32, 72f32, 0f32, 15f32, 0f32, 0f32, 896f32, 0f32 ]
--				   [ 72f32, 48f32, 0f32, 90f32 ]
--		  }

let histo_mul [w][n] (is: [n]i64) (vs: [n]f32, hist: [w]f32) : [w]f32 =
  let hist' = map2 (*) hist hist
  in  reduce_by_index hist' (*) 1.0f32 is vs

entry main [n][w] (is: [n]i64) (vs: [n]f32) (hist: *[w]f32) (hist_bar: [w]f32) =
  vjp (histo_mul is) (vs,hist) hist_bar