-- ==
-- compiled random input { [200][10]f32 [200][10]f32 [10]f32 } auto output

def main [n1] [n2] [m] (X1: [n1][m]f32) (X2: [n2][m]f32) (Y: [m]f32) : [][]f32 =
  let res =
    map (\x1 ->
           (map (\x2 ->
                   #[sequential]
                   let Y0 = Y
                   let Y1 = map2 (\x y -> y + 3 * x) x1 Y
                   let a = reduce (+) 0 x2
                   let Y2 = map2 (\x y -> y + a * x) x2 Y1
                   in [ f32.i64 0
                      , f32.i64 1
                      , (Y2[0])
                      , (Y2[1])
                      , (Y2[2])
                      , (Y2[3])
                      , (Y2[4])
                      , (Y2[5])
                      , (Y2[6])
                      , (Y2[7])
                      , (Y2[8])
                      , (Y2[9])
                      ])
                X2))
        X1
  in (flatten res)
