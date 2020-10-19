-- ==
--
-- compiled random input {[256][256][16][16]f32 [256][256][16][16]f32} auto output
--
-- compiled random input {[64][128][32][32]f32 [64][128][32][32]f32} auto output

let main [a][b][m][n][q]  (A0: [a][b][m][q]f32) 
                          (B0: [a][b][q][n]f32)
                        : [a][b][m][n]f32 =
 map2(\A1 B1 ->
  map2(\A B -> 
    map(\Arow ->
            map (\Bcol ->
                    let r = map2 (\x y -> x*y) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  r
                ) (transpose B)
        ) A
  ) A1 B1
 ) A0 B0
  
