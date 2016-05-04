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
fun int tupfun( (int,(f64,int)) x, (int,(f64,int)) y ) =
    let (x1, x2) = x in
    let (y1, y2) = y in
        x1 + y1
    --let (x0, (x1,x2)) = x in
    --let (y0, (y1,y2)) = y in
    --33

fun int main(int x1, f64 y1, int z1, int x2, f64 y2, int z2) =
    tupfun((x1,(y1,z1)),(x2,(y2,z2)))
