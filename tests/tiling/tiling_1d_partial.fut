-- Tiling a redomap when not all of the arrays are invariant.
-- ==
-- compiled random input { [256][256]f32 [256]f32 } auto output
-- compiled random input { [256][10]f32 [256]f32 } auto output
-- structure gpu { SegMap/Loop/SegMap 2 }

def dotprod [n] (xs: [n]f32) (ys: [n]f32): f32 =
  reduce (+) 0.0 (map2 (*) xs ys)

def main [n][m] (xss: [m][n]f32) (ys: [m]f32) =
  -- The transpose is to avoid a coalescing optimisation that would
  -- otherwise make manifestation of a transpose the bottleneck.
  map (\xs -> #[sequential] dotprod ys xs) (transpose xss)
