def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let iota_n = iota n
  let tflgs_rot = map (\i -> if i==0 then 0 else tflgs[i-1]) iota_n
  let indsT = scan (+) 0 tflgs_rot
  -- ^ This is nasty. Since tflogs_rot gets substituted before rewriting
  -- the scan, the first case is (i == 0 => 0), not (i == 0 => tflgs_rot[0]),
  -- and so the scan is not recognised as a sum.
  in indsT
  -- let tmp   = scan (+) 0 fflgs
  -- let lst   = if n > 0 then indsT[n-1] + tflgs[n-1] else 0
  -- let indsF = map (\t -> t +lst) tmp
  -- let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  -- in  inds
