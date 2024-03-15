-- def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
--   let tflgs = map (\c -> if c then 1 else 0) conds
--   let iota_n = iota n
--   let tflgs_rot = map (\i -> if tflgs[i]==0 then 0 else tflgs[i-1]) iota_n
--   in tflgs_rot
def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let iota_n = iota n
  let tflgs_rot = map (\i -> if tflgs[if i <= -1 then 1337 else i]==0 then 0 else tflgs[i-1]) iota_n
  in tflgs_rot
