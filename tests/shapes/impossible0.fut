-- This program should not be permitted, as it is not computable.
-- ==
-- error: unknowable size

let shape [n] [m] (xss: [n][m]i32) = (n, m)

let main (b: bool) (xs: []i32) =
  let arr = []
  in if b
     then shape ([replicate xs[0] 0] ++ arr)
     else shape arr
