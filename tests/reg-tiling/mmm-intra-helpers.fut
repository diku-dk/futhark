entry mk_attention_like_input p q m n k : ([p][m][k]f16, [p][q][k][n]f16) =
  (rep 1f16 |> unflatten_3d, rep 1f16 |> unflatten_4d)

let dotproduct x y =
  map2 (*) x y
  |> map f32.f16
  |> reduce (+) 0

let matmul16 (A: [16][16]f16) (B: [16][16]f16) : [16][16]f32 =
    map (\ Arow ->
        map (\Bcol ->
            dotproduct Arow Bcol)
        (transpose B)
    ) A

let matmul [m][n][k] (A: [m][k]f16) (B: [k][n]f16) =
    map (\ Arow ->
        map (\Bcol ->
            dotproduct Arow Bcol)
        (transpose B)
    ) A

let matmul64 (A: [64][64]f16) (B: [64][64]f16) : [64][64]f32 =
    map (\ Arow ->
        map (\Bcol ->
            dotproduct Arow Bcol)
        (transpose B)
    ) A
