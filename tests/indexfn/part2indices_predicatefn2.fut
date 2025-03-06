def part2indices [n] (p: [n]f32 -> [n]bool) (xs : [n]f32)
  : {[n]i64 | \inds ->
        let conds = p xs
        in filtPartInv inds (\_i -> true) (\i -> conds[i])
    } =
  let conds = p xs
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  inds
