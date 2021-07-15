let main [n] (xs: [n]i32) =
  let ys =
    loop xs = replicate 10 n for i < n do
      if i % 2 == 0 then
        xs
      else
        replicate 10 i
  in ys
