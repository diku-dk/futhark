def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let iota_n = iota n
  let tflgs_rot = map (\i -> if i==0 then 0 else tflgs[i-1]) iota_n
  -- XXX once the above works do
  -- let tflgs_rot = map (\i -> if i==0 then 0 else tflgs[i-1]) (iota n)
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] + tflgs[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  inds
