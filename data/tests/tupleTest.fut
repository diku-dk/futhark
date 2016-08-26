-- Test various abuse of tuples - specifically, the flattening done by
-- internalisation.
-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }

fun f(x: (int,int)): (int,int) = x

fun main(): (int,int) =
    let x = 1 + 2        in
    let y = (x + 5, 4+7) in
    let (z, (t,q)) = (x, y) in
        f(y)
