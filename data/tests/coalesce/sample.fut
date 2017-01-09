fun main():[]i32 =
  let xs = iota(10)
  let as = map (fn (x:i32):i32 => x*x) xs
  in as
