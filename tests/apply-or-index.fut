-- Test that we can distinguish function application with literal
-- array argument from array indexing.  == input { 1 } output { 3 }

fun f(xs: []i32): i32 = xs[0]

val a: []i32 = [1,2,3]

fun main(x: i32): i32 =
  f [x] + a[x]
