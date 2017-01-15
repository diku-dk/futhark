-- Wrong, because the map function must return an array.
-- ==
-- error:

fun main(a: []i32): ([]i32) = map (\(x: i32): i32  -> x + 2) a
