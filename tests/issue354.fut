-- ==
-- structure { Screma 6 }

def linerp2D (image: [][]f32) (p: [2]i32) : f32 =
  #[unsafe]
  let a = p[0]
  let b = p[1]
  in image[a, b]

def f [n] (rotSlice: [n][n]f32) : [n][n]f32 =
  let positions1D = iota n
  let positions2D = map (\x -> map (\y -> [i32.i64 x, i32.i64 y]) positions1D) positions1D
  in map (\row -> map (linerp2D rotSlice) row) positions2D

def main [s] [n] (proj: [s][n]f32) : [s][n][n]f32 =
  let rotatedVol = map (\row -> map (\col -> replicate n col) row) proj
  in map (\x -> f x) rotatedVol
