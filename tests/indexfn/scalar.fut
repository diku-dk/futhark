def f (x: i64) : {[x]i64 | \_ -> true} =
  let y = replicate x (2*x)
  in y
