let matmult [n][m][p] (x: [n][m]f32) (y: [m][p]f32) : [n][p]f32 =
  map (\xr ->
    map (\yc ->
      reduce (+) 0 (map2 (*) xr yc)
        ) (transpose y)
      ) x

let main [n][m] (x: [n][m]f32) : [][]f32 =
  loop x for i < m-1 do
    matmult x[1:,:] x[:,1:]
