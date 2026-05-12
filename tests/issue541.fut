def f =
  \x ->
    let h y = y
    in h x

entry main1 (x: i32) = f x + f x
