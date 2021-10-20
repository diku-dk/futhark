-- ==
-- structure gpu { SegMap 1 }

let main iss =
  map (\is ->
         let acc = replicate 3 0
         let acc[is[0]] = 1
         let acc[is[1]] = 2
         let acc[is[2]] = 3
         in acc)
      iss
