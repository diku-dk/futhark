-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }
fun f(x: (int,int), y: int, z: int): (int,int) = x

fun main(): (int,int) =
    let x = 1 + 2        in
    let x = (x + 5, 4+7) in
    let (x, (y,z)) = (x, x) in
        f(x,y,z)
