let main [n] (xs: [n]i32) =
  let xss = map (\x ->
                   if x % 2 == 0 then
                     replicate n 0
                   else
                     replicate n 1) xs
  in xss
