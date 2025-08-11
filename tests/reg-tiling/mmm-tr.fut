-- ==
--
-- compiled random input {[2001][4037]f32 [2021][4037]f32} auto output
-- compiled random input {[1024][1024]f32 [1024][1024]f32} auto output

-- compiled random input {[2048][4096]f32 [2048][4096]f32} auto output
-- compiled random input {[2011][4011]f32 [1011][4011]f32} auto output
--

def dotproduct [n] (x: [n]f32) (y: [n]f32) =
  map2 (*) x y |> reduce (+) 0

def main [m] [n] [q] (A: [m][q]f32) (B: [n][q]f32) : [m][n]f32 =
  map (\Arow -> map (\Brow -> dotproduct Arow Brow) B) A
