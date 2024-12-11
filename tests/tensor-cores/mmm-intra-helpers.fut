def dotproduct_f16_f32 x y =
  map2 (*) x y
  |> map f32.f16
  |> reduce (+) 0.0f32
            
def dotproduct_f16 x y =
  map2 (*) x y
  |> reduce (+) 0.0f16

              
def matmul_f16_f32 [m][n][k] (A: [m][k]f16) (B: [k][n]f16) =
    map (\ Arow ->
        map (\Bcol ->
            dotproduct_f16_f32 Arow Bcol)
        (transpose B)
    ) A

def matmul_f16 [m][n][k] (A: [m][k]f16) (B: [k][n]f16) =
    map (\ Arow ->
        map (\Bcol ->
            dotproduct_f16 Arow Bcol)
        (transpose B)
    ) A
    
