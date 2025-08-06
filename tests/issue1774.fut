-- ==
-- compiled random input { [3]f32 [100]f32 [10]f32 } auto output
-- structure mc-mem { Alloc 3 }

def f [n] [m] [l] (xs: [n]f32) (ys: [m]f32) (zs: *[l]f32) : [n][m]f32 =
  map (\x ->
         map (\y -> f32.sum (map (* y) (map (* x) zs)))
             ys)
      xs

entry main [n] [m] [l] : [n]f32 -> [m]f32 -> *[l]f32 -> [n][m]f32 = f
