def dotproduct x y = 
  map2 (*) x y
  |> map f32.f16
  |> reduce (+) 0

def matmul [m][n][k] (A: [m][k]f16) (B: [k][n]f16) =
    map (\ Arow -> 
        map (\Bcol -> 
            dotproduct Arow Bcol) 
        (transpose B)
    ) A     
