-- This one screwed up multi-versioning at one point.  The problem was
-- that loop interchange produced identity maps.

def main [n] [m] (xss: *[n][m]i32) =
  map (\(xs: []i32) ->
         let ys = copy xs
         let (xs, _) =
           loop (zs: [m]i32, ys: [m]i32) = (xs, ys)
           for i < n do
             let xs' = scatter (copy ys) (iota m) (rotate 1 zs)
             in (xs', zs)
         in xs)
      xss
