-- Yet another case of aliasing that can result in incorrect code
-- generation.

def main [m] [n] (xi_0: [m][n]f32) (xi_1: [m][n]f32) =
  map2 zip xi_0 xi_1
