-- ==
-- input { [2i64,2i64,3i64, 7i64, 8i64] }
-- auto output

def main (xs: []i64) =
  map (\x ->
         let ys = iota x
         let reps = opaque (replicate x 2 with [1] = x)
         let ks = opaque ([reps, ys])
         let ks' = map (map (* 2)) ks
         in map (i64.sum) ks')
      xs
