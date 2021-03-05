-- Testing for dynamic stencil index in 2D
-- ==
--
-- input { [[1,2,3,4,5],[5,4,3,2,1]] [-1,1] }
-- output { [[5,4,4,4,5],[5,4,4,4,5]] }

let main [n][m] (arr : [n][m]f32) (ixs : [2]i64) =
  let ixss = zip ixs ixs
  let f _ xs = xs[0] + xs[1]
  in stencil_2d ixss f (map (map (const ())) arr) arr