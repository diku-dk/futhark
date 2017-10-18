-- If the reduction function accumulator type is unique, consume the
-- initial value.
-- ==
-- error: .*consumed.*

let main(a: *[]i32): []i32 =
  let b = reduce (\(acc: *[]i32) (i: []i32): *[]i32  -> acc) a (replicate 10 (iota(10))) in
  map (+) a b -- Should fail, because a has been consumed!
