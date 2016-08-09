-- ==
-- input {
--   3 [0,2,4,6,8,10]
-- }
-- output {
--   90
--   [90, 96, 102]
--   [ [10, 11, 12 ]
--   , [22, 24, 26 ]
--   , [36, 39, 42 ]
--   , [52, 56, 60 ]
--   , [70, 75, 80 ]
--   , [90, 96, 102]
--   ]
--   [99.0, 216.0, 351.0, 504.0, 675.0, 864.0]
--   [351.0, 504.0, 675.0, 864.0]
--   [10, 22, 36, 52, 70, 90]
-- }
fun ((int,[]int),[][]int,[]f64,[]f64,[]int) main(int m, *[n]int as) =
  let b = map(+10, as) in
  streamSeq( fn ((int,[]int),[][m]int,[]f64,[]f64,[]int) (int chunk, (int, [m]int) acc2, *[]int c) =>
                    let (acc0, acc) = acc2                in
                    let x = map ( fn []int (int c) =>
                                    map(+c, iota(m))
                                , c )                     in
                    let y0= scan( fn []int ([]int acc, []int x) =>
                                    zipWith(+, acc, x)
                                , replicate(m,0), x )     in
                    let y = map ( fn []int ([]int y0) =>
                                    zipWith(+, acc, y0)
                                , y0 )                    in
                    let z = map ( fn f64 ([]int y) =>
                                    let rs = map( fn f64 (int u) =>
                                                    f64(3*u)
                                                , y )
                                    in  reduce(+, 0.0, rs )
                                , y )                     in
                    let w = filter( fn bool (f64 z) =>
                                        (z / 55.0) > 4.0
                                  , z )                   in
--                    let D = scan (+, 0, c)                in
--                    let E = map (+acc0, D)                in
                    -- c becomes scan + 0 c
                    let c[0] = c[0] + acc0                in
                    loop (c) =
                        for j < chunk-1 do
                            let c[j+1] = c[j+1] + c[j]
                            in  c
                    in
                    ( (c[chunk-1],y[chunk-1]), y, z, w, c )

           , (0,replicate(m,0)), b )
