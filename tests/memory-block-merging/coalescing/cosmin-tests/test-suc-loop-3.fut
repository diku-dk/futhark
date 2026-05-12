-- ==
-- structure gpu-mem { Alloc 4 }
-- structure seq-mem { Alloc 2 }

def main [n] (ind: i64) (ass: [n][n]i64) =
  let yss = map (map (+ 1)) ass |> opaque
  -- replicate n (replicate n 2)
  let cs = map (reduce (+) 0) yss
  let xs = replicate n 0i64 |> opaque
  let xs[0] = cs[0]
  let xs =
    loop (xs)
    for i < n - 1 do
      let xs[i + 1] = 3 * xs[i] + cs[i] * i
      in xs
  let yss[ind] = xs
  in yss
