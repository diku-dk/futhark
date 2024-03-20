def part2Indices [n] 't (conds: [n]bool) : {[n][1][2]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> let x = if c
                                 then [1, if 1 > 2 then 1337 else 100]
                                 else [0, 0]
                         let y = [x]
                         in y
                  ) conds
  in tflgs
