-- Intra-block reduction with an array-typed operator, over an index space wide
-- enough that the block reduction takes more than one step - unlike
-- reduce4.fut, which has only two elements per segment.
-- ==
-- entry: single_segment
-- random input { [2][64][2]i32 } auto output
-- random input { [2][1000][2]i32 } auto output
-- random input { [2][0][2]i32 } auto output
-- structure gpu { SegMap/SegRed 3 }

-- ==
-- entry: segmented
-- random input { [2][5][64][2]i32 } auto output
-- random input { [2][5][0][2]i32 } auto output

-- The result is read by every thread before the map writes its own result,
-- which may be put in the same memory.
-- ==
-- entry: then_map
-- random input { [2][64][2]i32 } auto output
-- random input { [2][0][2]i32 } auto output

def op [m] (xs: [m]i32) (ys: [m]i32) : [m]i32 =
  loop zs = replicate m 0i32
  for i < m do
    let zs[i] = xs[i] + ys[i]
    in zs

entry single_segment xsss =
  #[flattening(only_intra)]
  map (reduce op (replicate 2 0i32)) xsss

entry segmented xssss =
  #[flattening(only_intra)]
  map (map (reduce op (replicate 2 0i32))) xssss

entry then_map xsss =
  #[flattening(only_intra)]
  map (\xss ->
         let r = reduce op (replicate 2 0i32) xss
         in map (\xs -> [xs[0] * r[1], xs[1] * r[0]]) xss)
      xsss
