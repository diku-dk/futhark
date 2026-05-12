-- The "fix" for this in the internaliser was actually a workaround
-- for a type checker bug (#1565).
-- ==
-- error: Initial loop values do not have expected type.

def matmult [n] [m] [p] (x: [n][m]f32) (y: [m][p]f32) : [n][p]f32 =
  map (\xr ->
         map (\yc ->
                reduce (+) 0 (map2 (*) xr yc))
             (transpose y))
      x

def main [n] [m] (x: [n][m]f32) : [][]f32 =
  loop x for i < m - 1 do
    matmult x[1:, :] x[:, 1:]
