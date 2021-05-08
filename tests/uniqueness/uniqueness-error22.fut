-- Test that we cannot consume anything inside an anonymous function.
-- ==
-- error: non-unique

let f(a: *[]i64) = a[0]

let main(n: i64): i32 =
  let a = iota(n) in
  reduce (\(sum: i32, i: i32): i32  -> sum + f(a)) 0 (iota(10))
