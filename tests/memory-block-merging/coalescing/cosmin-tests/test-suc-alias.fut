-- ==
-- structure gpu-mem { Alloc 4 }
-- structure seq-mem { Alloc 1 }

def main [n] (ind: i64) (ass: [n][n]i64) =
  let np1 = n + 1
  let yss = replicate np1 (replicate np1 2)
  let as = map (reduce (+) 0) ass
  let bs = opaque as
  let cs = as[2:]
  let ds = bs[:n - 2]
  let r1 = reduce (+) 0 cs
  let r2 = reduce (+) 0 ds
  let yss[ind, 1:] = bs
  in (r1, r2, yss)
