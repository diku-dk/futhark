-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error:

fun main(a: *[]int): []int =
  let b = scan (fn (acc: *[]int, i: *[]int): *[]int  => acc) a (replicate(10,iota(10))) in
  size(0,a)+size(0,b) -- Should fail, because a has been consumed!
