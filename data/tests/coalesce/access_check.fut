fun main(): []i32 =
  let x1 = [1,2,3,4,5,6,7,8,9]
  let x2 = [1,2,3,4,5,6,7,8,9]
  let a = map (fn (x:i32):i32 =>
    x1[x] + x2[x]
  ) (iota(9)) in
  a
