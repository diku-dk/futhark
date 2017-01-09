fun main(): []i32 =
  let n = 3
  let x1 = iota(n)
  let x2 = iota(n)
  let a = zip x1 x2 in
  a
