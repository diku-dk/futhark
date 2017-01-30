-- Do user-defined operators have the right associativity?
-- ==
-- input { 1 2 3 } output { true }

fun (x: i32) &-& (y: i32) = x - y

fun main(x: i32, y: i32, z: i32): bool =
  x &-& y &-& z ==
  (x &-& y) &-& z
