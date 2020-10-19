-- ==
--
-- compiled random input {[1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input {[2048][4096]f32 [4096][2048]f32} auto output
--
-- input {
--   [ [1.0f32, 2.0, 3.0], [3.0, 4.0, 5.0] ]
--   [ [1.0f32, 2.0], [3.0, 4.0], [5.0, 6.0] ]
-- }
-- output {
--   [ [22.0f32, 28.0], [40.0, 52.0] ]
-- }
--


let main [m][n][q]  (A: [m][q]f32) 
                    (B: [q][n]f32)
                  : [m][n]f32 =
    map(\Arow ->
            map (\Bcol ->
                    let r = map2 (\a b -> a*b) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  r
                ) (transpose B)
        ) A

let main_inst (A: [1024][1024]f32) 
         (B: [1024][1024]f32)
         : [1024][1024]f32 =
    map(\Arow ->
            map (\Bcol ->
                    let r = map2 (\a b -> a*b) Arow Bcol |>
                            reduce (+) 0.0f32
                    in  r
                ) (transpose B)
        ) A
