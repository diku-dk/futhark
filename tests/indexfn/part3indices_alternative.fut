def partition3_indices [n] 't (p: t -> bool) (q: t -> bool) (xs: [n]t)
    : {(i64, i64, [n]i64) | \(_,_,is) ->
       FiltPartInv2 is (\_i -> true) (\i -> p xs[i]) (\i -> !p xs[i] && q xs[i])
    } =
  let tflags = map (\x -> if p x then 1 else 0 ) xs
  let eflags = map (\x -> if !(p x) && q x then 1 else 0 ) xs

  let indsL = scan (+) 0 tflags
  let indsE = scan (+) 0 eflags

  let a = if n > 0 then indsL[n-1] else 0
  let b = if n > 0 then indsE[n-1] else 0

  let inds  = map4 (\ x indL indE i ->
                        if p x then indL - 1
                        else if q x then a + indE - 1
                        else a + b + i - indsL[i] - indsE[i]
                   ) xs indsL indsE (iota n)
  in  (a, b, inds)
