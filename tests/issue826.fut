let main (xss: [][]i32) =
  map (\xs ->
         let sum = i32.sum xs
         let xs' = copy xs
         let xs'[0] = sum
         let xs'[1] = sum
         in xs')
      xss
