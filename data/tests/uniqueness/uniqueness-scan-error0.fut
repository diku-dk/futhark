-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error:

fun []int main(*[]int a) =
  let b = scan(fn *[]int (*[]int acc, *[]int i) => acc, a, replicate(10,iota(10))) in
  size(0,a)+size(0,b) -- Should fail, because a has been consumed!
