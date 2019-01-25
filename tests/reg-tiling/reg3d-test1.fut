-- Test register tiling when all input arrays are invariant to one parallel dimension
-- This is a simple case in which there is no code after stream.
--
-- ==
-- input {
--   [ [1.0f32, 3.0], [2.0, 4.0] ]
--   [ [5.0f32, 8.0], [6.0, 7.0] ]
--   [ [1.0f32, 1.0], [9.0, 1.0] ]
-- }
-- output { 
--   [ [ [23.0f32, 29.0], [34.0, 44.0] ]
--   , [ [18.0f32, 21.0], [24.0, 28.0] ]
--   ]
-- }

-- compile input @ data/medium.in
-- output @ data/medium-1.out

let pred (x : f32) : bool = x < 9.0

let dotprod_filt [n] (vct: [n]f32) (xs: [n]f32) (ys: [n]f32) : f32 =
  f32.sum (map3 (\v x y -> let z = x*y in let f = f32.bool (pred v) in z*f) vct xs ys)

let matmul_filt [n][p][m] (xss: [n][p]f32) (yss: [p][m]f32) (vct: [p]f32) : [n][m]f32 =
  map (\xs -> map (dotprod_filt vct xs) (transpose yss)) xss

let main [m][n][u]  (ass: [n][u]f32) 
                    (bss: [u][n]f32) 
                    (fss: [m][u]f32) : 
                    [m][n][n]f32 =
    map (matmul_filt ass bss) fss
