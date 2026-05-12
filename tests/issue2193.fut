def takefrom 't (xs: []t) (i: i64) : [i + 1]t = take (i + 1) xs

entry main n (xs: []i32) =
  n |> takefrom xs
