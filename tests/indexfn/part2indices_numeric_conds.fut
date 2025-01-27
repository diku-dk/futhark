-- def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
def part2Indices [n] 't (conds: [n]i64) : {[n]i64 | \_ -> true} =
  -- let tflgs = map (\c -> i64.bool c) conds
  let tflgs = map (\c -> if c == 1 then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c == 1 then indT-1 else indF-1) conds indsT indsF
  in  inds
