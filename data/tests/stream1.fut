-- ==
-- input {
--   3 [0,2,4,6,8,10]
-- }
-- output {
--   ( 90
--   , [90, 96, 102]
--   , [ [10, 11, 12 ]
--     , [22, 24, 26 ]
--     , [36, 39, 42 ]
--     , [52, 56, 60 ]
--     , [70, 75, 80 ]
--     , [90, 96, 102]
--     ]
--   , [99.0, 216.0, 351.0, 504.0, 675.0, 864.0]
--   , [351.0, 504.0, 675.0, 864.0]
--   , [10, 22, 36, 52, 70, 90]
--   )
-- }
fun ((int,[int]),[[int]],[f64],[f64],[int]) main(int m, *[int,n] A) =
  let B = map(+10, A) in
  streamSeq( fn ((int,[int]),[[int,m]],[f64],[f64],[int]) (int chunk, (int, [int,m]) acc2, *[int] C) =>
                    let (acc0, acc) = acc2                in
                    let X = map ( fn [int] (int c) =>
                                    map(+c, iota(m))
                                , C )                     in
                    let Y0= scan( fn [int] ([int] acc, [int] x) =>
                                    zipWith(+, acc, x)
                                , replicate(m,0), X )     in
                    let Y = map ( fn [int] ([int] y0) =>
                                    zipWith(+, acc, y0)
                                , Y0 )                    in
                    let Z = map ( fn f64 ([int] y) =>
                                    let rs = map( fn f64 (int u) =>
                                                    f64(3*u)
                                                , y )
                                    in  reduce(+, 0.0, rs )
                                , Y )                     in
                    let W = filter( fn bool (f64 z) =>
                                        (z / 55.0) > 4.0
                                  , Z )                   in
--                    let D = scan (+, 0, C)                in
--                    let E = map (+acc0, D)                in
                    -- C becomes scan + 0 C
                    let C[0] = C[0] + acc0                in
                    loop (C) =
                        for j < chunk-1 do
                            let C[j+1] = C[j+1] + C[j]
                            in  C
                    in
                    ( (C[chunk-1],Y[chunk-1]), Y, Z, W, C )

           , (0,replicate(m,0)), B )
