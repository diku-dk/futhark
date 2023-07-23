-- Intra-group reduction with array accumulator.
-- ==
-- random input { [2][2][128]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

def badsum [n][m] (xss: [n][m]i32): []i32 =
  reduce_comm(\(xs: []i32) ys ->
               loop zs = replicate m 0 for i < m do
                 let zs[i] = xs[i] + ys[i]
                 in zs)
             (replicate m 0) xss

def main xs =
  #[incremental_flattening(only_intra)]
  map badsum xs
