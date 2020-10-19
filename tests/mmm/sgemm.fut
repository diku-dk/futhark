-- ==
-- input {
--   2.0f32
--   3.0f32
--   [ [1.0f32, 2.0, 3.0], [3.0, 4.0, 5.0] ]
--   [ [1.0f32, 2.0], [3.0, 4.0], [5.0, 6.0] ]
--   [ [1.0f32, 2.0], [3.0, 4.0] ]
-- }
-- output {
--   [ [47.0f32, 62.0], [89.0, 116.0] ]
-- }
--
-- compiled random input {f32 f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input {f32 f32 [2048][4096]f32 [4096][2048]f32 [2048][2048]f32} auto output


let main [m][n][q] (alpha: f32) 
                    (beta : f32) 
                    (A: [m][q]f32) 
                    (B: [q][n]f32)
                    (C: [m][n]f32) : [m][n]f32 =
    map2(\Arow Crow ->
            map2(\Bcol c ->
                    let r = map2 (\a b -> a*b) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  alpha * r + beta * c
                ) (transpose B) Crow
        ) A C
