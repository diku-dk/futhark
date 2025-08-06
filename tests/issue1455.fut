entry test [n] (xs: *[n]i32) : (*[]i32, *[]i32) =
  let a = [0]
  let cp =
    loop a for i < n do
      copy xs[i:i + 1]
  in (cp, xs)
