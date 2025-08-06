-- Reshape with a polymorphic type, where only the outer dimensions
-- are reshaped.

def main [n] [m] [k] (A: [n][m][k]f32) : []i32 =
  let A' = flatten A
  in map (\_ -> 0) A'
