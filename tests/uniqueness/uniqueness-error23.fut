-- ==
-- error: .*consumed.*

let g(ar: *[]i32, a: *[][]i32): i32 =
  ar[0]

let f(ar: *[]i32, a: *[][]i32): i32 =
  g(a[0], a) -- Should be a type error, as both are supposed to be unique

let main(n: i32): i32 =
  let a = copy(replicate n (iota n))
  let ar = copy(a[0]) in
  f(ar, a)
