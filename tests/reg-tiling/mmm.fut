-- ==
-- compiled random input {[2048][4096]f32 [4096][2048]f32} auto output

-- compiled random input {[2011][4011]f32 [4011][1011]f32} auto output
-- compiled random input {[128][1024]f32 [1024][128]f32} auto output
-- compiled random input {[128][4096]f32 [4096][128]f32} auto output
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

let main [m][n][q]  (A: [m][q]f32) (B: [q][n]f32) : [m][n]f32 =
    map (\ Arow -> map (\Bcol -> dotproduct Arow Bcol) (transpose B)) A

