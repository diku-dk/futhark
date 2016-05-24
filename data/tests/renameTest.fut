-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }
fun (int,int) f((int,int) x, int y, int z) = x

fun (int,int) main() =
    let x = 1 + 2        in
    let x = (x + 5, 4+7) in
    let (x, (y,z)) = (x, x) in
        f(x,y,z)
