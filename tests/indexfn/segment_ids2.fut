let sum_i64 [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

type nat_i64 = {i64 | (>= 0)}

def sgm_sum [n] 't
      (flags: [n]bool)
      (xs: [n]i64): {[n]i64 | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys

-- Expands a shape array to a flat array of segment ids.
def mkII [n] (S: [n]nat_i64) : {[]i64 | \_ -> true} =
   let B     = scan (+) 0 S   -- Futhark's inclusive scan
   let inds  = map2 (\ b i -> if i < n-1 && S[i+1] == 0 then -1 else b) B (iota n)
   let zeros = replicate B[n-1] 0
   let ids = map (\i -> i + 1) (iota n)
   let flags = scatter zeros inds ids
   let flags_bool = map (\f -> f > 0) flags
   let II    = sgm_sum flags_bool flags
   in  II
