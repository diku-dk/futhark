-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

def main [n] (ind: i64) (q: f32) (ass: [n][n]f32) (as: [n]f32) =
  let yss = map (map (+ 1)) ass
  -- replicate n (replicate n 2) -- map (map (+1)) ass
  let bs =
    if (q > 0)
    then let b1s = map (+ 2) as
         -- replicate n 2 -- map (+2) as
         in b1s
    else let b2s = map (+ 3) as
         -- replicate n 3 -- map (+3) as
         in b2s
  let yss[ind] = bs
  in yss
