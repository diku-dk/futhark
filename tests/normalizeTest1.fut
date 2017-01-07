-- ==
-- input {
--   1
--   2.0
--   3
--   4
--   5.0
--   6
-- }
-- output {
--   5
-- }
fun tupfun(x:  (int,(f64,int)), y: (int,(f64,int)) ): int =
    let (x1, x2) = x
    let (y1, y2) = y in
        x1 + y1
    --let (x0, (x1,x2)) = x in
    --let (y0, (y1,y2)) = y in
    --33

fun main(x1: int, y1: f64, z1: int, x2: int, y2: f64, z2: int): int =
    tupfun((x1,(y1,z1)),(x2,(y2,z2)))
