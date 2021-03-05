-- Testing for static stencil index in 2D
-- ==
--
-- input { [[1,2,3,4,5],[5,4,3,4,5]] }
-- output { [[5,4,4,4,5],[5,4,4,4,5]] }

let main [n][m] (arr : [n][m]f32) : [n][m]f32 =
  let f _ xs = xs[0] + xs[1]
  in stencil_2d [(-1,-1),(1,1)] f (map (map (const ())) arr) arr
