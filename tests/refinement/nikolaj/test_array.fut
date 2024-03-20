def part2Indices [n] 't (conds: [n]bool) : {[n][3]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c
                         then [1, if c then 1337 else 100, if c then 1000 else 1001]
                         else [0, 0, 0]
                  ) conds
  in tflgs
