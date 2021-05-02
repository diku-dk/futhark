-- Testing for static stencil index in 2D
-- ==
--
-- input { [[1,2,3,4,5],[5,4,3,2,1]] }
-- output { [[5,4,4,4,5],[5,4,4,4,5]] }
-- random input { [1000][1000]i32 }
-- auto output

let main [n][m] (arr : [n][m]i32) : [n][m]i32 =
  let f _ xs = xs[0] + xs[1]
  in stencil_2d [(-1,-1),(1,1)] f (map (map (const ())) arr) arr
