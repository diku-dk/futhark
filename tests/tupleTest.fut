-- Test various abuse of tuples - specifically, the flattening done by
-- internalisation.
-- ==
-- input {
-- }
-- output {
--   8
--   11
-- }

fun f(x: (i32,i32)): (i32,i32) = x

fun main(): (i32,i32) =
    let x = 1 + 2
    let y = (x + 5, 4+7)
    let (z, (t,q)) = (x, y) in
        f(y)
