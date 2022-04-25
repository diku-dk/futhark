def main b (xs: *[2][3]i64) =
  let v = if b then iota 3 else copy xs[1]
  in xs with [0] = v
