-- ==
-- input {
--   [0,2,4,6,8,10]
-- }
-- output {
--   54
--   [12.0, 30.0, 54.0, 84.0, 120.0, 162.0]
-- }
fun main(as: *[]int): (int,[]f64) =
  streamSeq (fn chunk acc a: (int,*[]f64)  =>
              let x = map  (+4) a
              let y0= scan (+) 0 x
              let y = map  (+acc) y0
              let z = map  (fn (a: int): f64  => f64(3*a)) y
              let u = y0[chunk-1]
              in ( acc+u, copy(z) )
           ) 0 as
