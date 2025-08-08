-- ==
-- structure gpu-mem { Alloc 6 }
-- structure seq-mem { Alloc 4 }

def main [n] (ind: i64) (ass: [n][n]i64) (as: [n]i64) =
  let yss = map (map (+ 1)) ass |> opaque
  -- replicate n (replicate n 2
  let bs = map (* 3) as |> opaque
  let bs =
    loop (bs)
    for i < n do
      let cs = map (* 2) bs
      let s = reduce (+) 0 cs
      in map (+ s) cs
  let yss[ind] = bs
  in yss
