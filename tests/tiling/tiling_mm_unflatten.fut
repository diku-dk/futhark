-- A matrix multiplication whose result is unflattened and then mapped
-- over, as when a convolution (expressed as a matrix multiplication)
-- is followed by an elementwise activation.  The multiplication must
-- still be tiled.
-- ==
-- compiled random input { [64][100]f32 [100][8][16]f32 } auto output
-- structure gpu { SegMap/Loop/SegMap 1 SegMap/SegMap 2 }

def main [n][k][h][w] (a: [n][k]f32) (b: [k][h][w]f32) : [n][h][w]f32 =
  let c = map (\xs -> map (\ys -> f32.sum (map2 (*) xs ys)) (transpose (map flatten b))) a
  in map (\r -> map (map (+ 1)) (unflatten r)) c
