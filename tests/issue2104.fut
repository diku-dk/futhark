type v3 = (f32, f32, f32)
type v4 = (f32, f32, f32, f32)
type C4N3V3 = (v4, v3, v3)

def serializeC4N3V3 (((r, g, b, a), (nx, ny, nz), (vx, vy, vz)): C4N3V3) = [r, g, b, a, nx, ny, nz, vx, vy, vz]

def triangleMeshC4N3V3 [m] [n] (vs: [m][n]C4N3V3) : []f32 =
  let triangle v0 v1 v2 = serializeC4N3V3 v0 ++ serializeC4N3V3 v1 ++ serializeC4N3V3 v2
  let quad v0 v1 v2 v3 = triangle v0 v1 v2 ++ triangle v2 v3 v0
  in tabulate_2d (m - 1) (n - 1) (\i j -> quad vs[i, j] vs[i + 1, j] vs[i + 1, j + 1] vs[i, j + 1])
     |> flatten
     |> flatten

entry particleSystemMesh [m] [n] [k]
                         (vs: [m][n]C4N3V3)
                         (coords: [k]v3) : []f32 =
  map (\_ -> vs) coords
  |> map triangleMeshC4N3V3
  |> flatten
