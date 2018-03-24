let main(n: i32) =
  let a = replicate n (replicate n 1)
  in map2 (\(xs: *[]i32) i -> xs with [0] <- i) a (iota n)
