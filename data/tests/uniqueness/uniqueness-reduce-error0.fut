-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error:

fun main(a: *[]int): []int =
  let b = reduce(fn (acc: *[]int, i: []int): *[]int  => acc, a, replicate(10,iota(10))) in
  map(op+, zip(a, b)) -- Should fail, because a has been consumed!
