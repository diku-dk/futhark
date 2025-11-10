-- Prelude
def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def partition [n] (p: f32 -> bool) (xs: [n]f32)
    : {(i64, [n]f32) | \(num_true, ys)->
      FiltPart ys xs (\_i -> true) (\i -> p xs[i])
        && num_true == sum (map (\x -> if p x then 1 else 0) xs)
    } =
  let conds = map (\x -> p x) xs
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  let ys = scatter (replicate n 0) inds xs
  in (lst, ys)
