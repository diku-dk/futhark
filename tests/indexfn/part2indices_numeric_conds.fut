def to_bool (i: i64) : bool = i == 1

def part2Indices [n] 't (conds: [n]i64)
    : {[n]i64 | \is -> FiltPartInv is (\_i -> true) (\i -> to_bool conds[i])} =
  let tflgs = map (\c -> if c == 1 then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c == 1 then indT-1 else indF-1) conds indsT indsF
  in  inds
