-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error: .*consumed.*

fun main(a: *[]int): []int =
  let b = scan (fn (acc: *[]int) (i: *[]int): *[]int  => acc) a (replicate 10 (iota(10))) in
  [a[0], b[0,0]] -- Should fail, because a has been consumed!
