def main [n][m] (xss: *[n][m]i32) =
  map (\(xs: [m]i32) ->
         let xs =
           loop (zs: [m]i32) = xs for i < n do
             let xs' = scatter (copy xs) (iota m) (rotate 1 zs)
             in xs'
         in xs) xss
