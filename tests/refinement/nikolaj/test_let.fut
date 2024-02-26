def part2Indices [n] 't (conds: [n]bool) : {[n][2]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> let x = if c
                                 then [1, if false then 1337 else 100]
                                 else [0, 0]
                         in x
                  ) conds
  in tflgs
