let mystery = false

def part2Indices [n] 't (conds: [n]bool) : {[n][2]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c
                         then [1, 2 + if mystery then 1337 else 100]
                         else [0, 0]
                  ) conds
  in tflgs
