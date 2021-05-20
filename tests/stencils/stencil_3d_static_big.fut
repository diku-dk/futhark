-- Testing for static large auto output
-- ==
--
-- compiled random input { [65][127][2025]f32 }
-- auto output

let main [n][m][k] (arr : [n][m][k]f32) : [n][m][k]f32 =
  let f _ xs = xs[0] + xs[1] + xs[2]
  in stencil_3d [(-1,-1,-1),  (0,0,0),  (1,1,1)] f (map (map (map (const ()))) arr) arr
