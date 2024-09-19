-- def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
def f [n] 't (conds: [n]bool) : [n]i64 =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let indsT = scan (+) 0 tflgs
  in indsT
