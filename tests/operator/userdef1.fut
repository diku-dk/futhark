-- Do user-defined operators have the right precedence?
-- ==
-- input { 2 3 4 } output { 14 }

fun (x: i32) +* (y: i32) = x * y

fun (x: i32) *+ (y: i32) = x + y

fun main(x: i32, y: i32, z: i32) = x +* y *+ z
