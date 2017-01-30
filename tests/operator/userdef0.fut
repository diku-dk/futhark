-- Can we define a user-defined operator at all?
-- ==
-- input { 2 3 } output { -1 }

fun (x: i32) + (y: i32) = x - y

fun main(x: i32, y: i32) = x + y