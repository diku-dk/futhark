let main [n] (xs: [n]i32) =
  let ys = if n % 2 == 0 then
             if n % 3 == 0 then
               if n % 4 == 0 then
                 if n % 5 == 0 then
                   replicate n 1
                 else
                   replicate n 11
               else
                 replicate n 10
             else
               replicate n 12
           else
             replicate n 15
  in map2 (+) xs ys
