-- ==
--
-- compiled random input {[1024][1024]f32 [1024][1024]f32} auto output
-- compiled random input {[2048][4096]f32 [2048][4096]f32} auto output
--

-- input {
--   [ [1.0f32, 2.0f32, 3.0f32], [3.0f32, 4.0f32, 5.0f32] ]
--   [ [1.0f32, 2.0f32], [3.0f32, 4.0f32], [5.0f32, 6.0f32] ]
-- }
-- output {
--   [ [22.0f32, 28.0f32], [40.0f32, 52.0f32] ]
-- }
--

let dotproduct [n] (x: [n]f32) (y: [n]f32) =
    map2 (*) x y |> reduce (+) 0

let main [m][n][q]  (A: [m][q]f32) (B: [n][q]f32) : [m][n]f32 =
    map (\ Arow -> map (\Brow -> dotproduct Arow Brow) B) A

