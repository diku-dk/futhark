let main xss =
  map (\xs ->
         loop xs = zip (copy xs) (copy xs)
         for i < 10 do xs with [0] = (xs[0].1 + 1,2))
      xss
