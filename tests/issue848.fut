type vector = {x: f32, y: f32, z: f32}
type triangle = (vector, vector, vector)

entry generate_terrain [depth] [width] (points: [depth][width]vector) =
  let n = width - 1
  let n2 = n * 2
  let triangles =
    let m = depth - 1
    in map3 (\row0 row1 i ->
               let (row0', row1') =
                 if i % 2 == 0
                 then (row0, row1)
                 else (row1, row0)
               in flatten (map4 (\c0 c0t c1 c1t ->
                                   let tri0 = (c0, c0t, c1)
                                   let tri1 = (c1, c1t, c0t)
                                   in [tri0, tri1])
                                (row0'[:width - 1] :> [n]vector)
                                (row0'[1:] :> [n]vector)
                                (row1'[:width - 1] :> [n]vector)
                                (row1'[1:] :> [n]vector))
                  :> [n2]triangle)
            (points[:depth - 1] :> [m][width]vector)
            (points[1:] :> [m][width]vector)
            ((0..<depth - 1) :> [m]i64)
  in triangles
