-- ==
-- input {
--   [0,2,4,6,8,10]
-- }
-- output {
--   54
--   [12.0, 30.0, 54.0, 84.0, 120.0, 162.0]
-- }
fun (int,[]f64) main(*[]int as) =
  streamSeq( fn (int,*[]f64) (int chunk, int acc, *[]int a) =>
                    let x = map (+4,   a ) in
                    let y0= scan(+, 0, x ) in
                    let y = map (+acc, y0) in
                    let z = map ( fn f64 (int a) => f64(3*a), y) in
                    let u = y0[chunk-1]
                    in ( acc+u, copy(z) )
           , 0, as )
