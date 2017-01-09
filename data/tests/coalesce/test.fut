fun main(): [][]i32 =
  let xs = [2,4,6,8,10]
  let ys = [1,3,5,7,9]
  let is = [0,1,2,3,4]
  let js = [0,1,2,3,4]
  let as = map (fn (i:i32) =>
    map (fn (j:i32) =>
      xs[i] + ys[j]
      ) is
    ) js
  in as
