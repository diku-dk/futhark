let mystery = false

def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> 1 + if c
                         then 1
                         else 0
                  ) conds
  in tflgs
