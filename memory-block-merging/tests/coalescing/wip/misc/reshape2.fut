-- Yet another reshape test program.
-- ==

-- structure cpu { Alloc 0 }
-- structure gpu { Alloc 0 }

import "/futlib/array"

let main [n] (x: [n][n]i32, i: i32): [][]i32 =
  let z = replicate n (replicate (n * n) 1)
  let y = map (\t -> map (* 2) t) x
  let y' = reshape (n * n) y
  let z[i] = y'
  in z
