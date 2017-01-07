-- Test that we can distinguish function application with literal
-- array argument from array indexing.  == input { 1 } output { 3 }

fun f(xs: []int): int = xs[0]

val a: []int = [1,2,3]

fun main(x: int): int =
  f [x] + a[x]
