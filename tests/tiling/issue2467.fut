-- Regression test for issue #2467: TileLoops must not produce invalid
-- IR when a redomap input array is also used in postlude code.
-- ==
-- compiled random input { [4][8]i32 [8][6]i32 [8][6]i32 } auto output

def dotprod [n] (xs: [n]i32) (ys: [n]i32) : i32 =
  #[sequential] i32.sum (map2 (i32.*) xs ys)

def matmul [n] [p] [m] (xss: [n][p]i32) (yss: [p][m]i32) : *[n][m]i32 =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

def matdiv_entrywise [m] [n] (xss: [m][n]i32) (yss: [m][n]i32) : *[m][n]i32 =
  map2 (map2 (i32./)) xss yss

entry main W A WH =
  let W_TA = matmul (transpose W) A
  let W_TWH = matmul (transpose W) WH
  let H_update = matdiv_entrywise W_TA W_TWH
  in H_update
